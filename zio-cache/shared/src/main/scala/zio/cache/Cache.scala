package zio.cache

import zio.internal.MutableConcurrentQueue
import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.{Exit, IO, Promise, Trace, UIO, URIO, Unsafe, ZIO}

import java.time.{Duration, Instant}
import java.util.Map
import java.util.concurrent.atomic.{AtomicBoolean, LongAdder}

/**
 * A `Cache` is defined in terms of a lookup function that, given a key of
 * type `Key`, can either fail with an error of type `Error` or succeed with a
 * value of type `Value`. Getting a value from the cache will either return
 * the previous result of the lookup function if it is available or else
 * compute a new result with the lookup function, put it in the cache, and
 * return it.
 *
 * A cache also has a specified capacity and time to live. When the cache is
 * at capacity the least recently accessed values in the cache will be
 * removed to make room for new values. Getting a value with a life older than
 * the specified time to live will result in a new value being computed with
 * the lookup function and returned when available.
 *
 * The cache is safe for concurrent access. If multiple fibers attempt to get
 * the same key the lookup function will only be computed once and the result
 * will be returned to all fibers.
 */
sealed abstract class Cache[-Key, +Error, +Value] {

  /**
   * Returns statistics for this cache.
   */
  def cacheStats(implicit trace: Trace): UIO[CacheStats]

  /**
   * Returns whether a value associated with the specified key exists in the
   * cache.
   */
  def contains(key: Key)(implicit trace: Trace): UIO[Boolean]

  /**
   * Returns statistics for the specified entry.
   */
  def entryStats(key: Key)(implicit trace: Trace): UIO[Option[EntryStats]]

  /**
   * Retrieves the value associated with the specified key if it exists.
   * Otherwise computes the value with the lookup function, puts it in the
   * cache, and returns it.
   */
  def get(key: Key)(implicit trace: Trace): IO[Error, Value]

  /**
   * Computes the value associated with the specified key, with the lookup
   * function, and puts it in the cache. The difference between this and
   * `get` method is that `refresh` triggers (re)computation of the value
   * without invalidating it in the cache, so any request to the associated
   * key can still be served while the value is being re-computed/retrieved
   * by the lookup function. Additionally, `refresh` always triggers the
   * lookup function, disregarding the last `Error`.
   */
  def refresh(key: Key): IO[Error, Unit]

  /**
   * Invalidates the value associated with the specified key.
   */
  def invalidate(key: Key)(implicit trace: Trace): UIO[Unit]

  /**
   * Invalidates all values in the cache.
   */
  def invalidateAll: UIO[Unit]

  /**
   * Returns the approximate number of values in the cache.
   */
  def size(implicit trace: Trace): UIO[Int]
}

object Cache {

  /**
   * Constructs a new cache with the specified capacity, time to live, and
   * lookup function.
   */
  def make[Key, Environment, Error, Value](
    capacity: Int,
    timeToLive: Duration,
    lookup: Lookup[Key, Environment, Error, Value]
  )(implicit trace: Trace): URIO[Environment, Cache[Key, Error, Value]] =
    makeWith(capacity, lookup, identity[Key])(_ => timeToLive)

  /**
   * Constructs a new cache with the specified capacity, time to live, and
   * lookup function, where the time to live can depend on the `Exit` value
   * returned by the lookup function.
   */
  def makeWith[Key, RealKey, Environment, Error, Value](
    capacity: Int,
    lookup: Lookup[Key, Environment, Error, Value],
    keyByInput: Key => RealKey
  )(
    timeToLive: Exit[Error, Value] => Duration
  )(implicit trace: Trace): URIO[Environment, Cache[Key, Error, Value]] =
    ZIO.clock.flatMap { clock =>
      ZIO.environment[Environment].flatMap { environment =>
        ZIO.fiberId.map { fiberId =>
          val cacheState = CacheState.initial[Key, RealKey, Error, Value]()
          import cacheState._

          def trackAccess(key: MapKey[Key]): Unit = {
            accesses.offer(key)
            if (updating.compareAndSet(false, true)) {
              var loop = true
              while (loop) {
                val key = accesses.poll(null)
                if (key ne null) {
                  keys.add(key)
                } else {
                  loop = false
                }
              }
              var size = map.size
              loop = size > capacity
              while (loop) {
                val key = keys.remove()
                if (key ne null) {
                  if (map.remove(key.value) ne null) {
                    size -= 1
                    loop = size > capacity
                  }
                } else {
                  loop = false
                }
              }
              updating.set(false)
            }
          }

          def trackHit(): Unit =
            hits.increment()

          def trackMiss(): Unit =
            misses.increment()

          new Cache[Key, Error, Value] {

            override def cacheStats(implicit trace: Trace): UIO[CacheStats] =
              ZIO.succeed(CacheStats(hits.longValue, misses.longValue, map.size))

            override def contains(k: Key)(implicit trace: Trace): UIO[Boolean] =
              ZIO.succeed(map.containsKey(k))

            override def entryStats(k: Key)(implicit trace: Trace): UIO[Option[EntryStats]] =
              ZIO.succeed {
                val value = map.get(k)
                if (value eq null) None
                else {
                  value match {
                    case MapValue.Pending(_, _) =>
                      None
                    case MapValue.Complete(_, _, entryState, _) =>
                      Option(EntryStats(entryState.loaded))
                    case MapValue.Refreshing(_, MapValue.Complete(_, _, entryState, _)) =>
                      Option(EntryStats(entryState.loaded))
                  }
                }
              }

            override def get(k: Key)(implicit trace: Trace): IO[Error, Value] = {
              val rk: RealKey = keyByInput(k)
              ZIO.suspendSucceedUnsafe { implicit u =>
                var key: MapKey[Key]               = null
                var promise: Promise[Error, Value] = null
                var value                          = map.get(k)
                if (value eq null) {
                  promise = newPromise()
                  key = new MapKey(k)
                  value = map.putIfAbsent(rk, MapValue.Pending(key, promise))
                }
                if (value eq null) {
                  trackAccess(key)
                  trackMiss()
                  lookupValueOf(k, promise)
                } else {
                  value match {
                    case MapValue.Pending(key, promise) =>
                      trackAccess(key)
                      trackHit()
                      promise.await
                    case MapValue.Complete(key, exit, _, timeToLive) =>
                      trackAccess(key)
                      trackHit()
                      if (hasExpired(timeToLive)) {
                        map.remove(rk, value)
                        get(k)
                      } else {
                        ZIO.done(exit)
                      }
                    case MapValue.Refreshing(
                          promiseInProgress,
                          MapValue.Complete(mapKey, currentResult, _, ttl)
                        ) =>
                      trackAccess(mapKey)
                      trackHit()
                      if (hasExpired(ttl)) {
                        promiseInProgress.await
                      } else {
                        ZIO.done(currentResult)
                      }
                  }
                }
              }
            }

            override def refresh(k: Key): IO[Error, Unit] = {
              val rk: RealKey = keyByInput(k)
              ZIO.suspendSucceedUnsafe { implicit u =>
                val promise = newPromise()
                var value   = map.get(k)
                if (value eq null) {
                  value = map.putIfAbsent(rk, MapValue.Pending(new MapKey(k), promise))
                }
                val result = if (value eq null) {
                  lookupValueOf(k, promise)
                } else {
                  value match {
                    case MapValue.Pending(_, promiseInProgress) =>
                      promiseInProgress.await
                    case completedResult @ MapValue.Complete(mapKey, _, _, ttl) =>
                      if (hasExpired(ttl)) {
                        map.remove(k, value)
                        get(k)
                      } else {
                        // Only trigger the lookup if we're still the current value, `completedResult`
                        lookupValueOf(mapKey.value, promise).when {
                          map.replace(rk, completedResult, MapValue.Refreshing(promise, completedResult))
                        }
                      }
                    case MapValue.Refreshing(promiseInProgress, _) =>
                      promiseInProgress.await
                  }
                }
                result.unit
              }
            }

            override def invalidate(k: Key)(implicit trace: Trace): UIO[Unit] =
              ZIO.succeed {
                map.remove(k)
                ()
              }

            override def invalidateAll: UIO[Unit] =
              ZIO.succeed {
                map.clear()
              }

            def size(implicit trace: Trace): UIO[Int] =
              ZIO.succeed(map.size)

            private def lookupValueOf(key: Key, promise: Promise[Error, Value]): IO[Error, Value] = {
              val rk = keyByInput(key)
              lookup(key)
                .provideEnvironment(environment)
                .exit
                .flatMap { exit =>
                  val now        = Unsafe.unsafeCompat(implicit u => clock.unsafe.instant())
                  val entryStats = EntryStats(now)

                  map.put(rk, MapValue.Complete(new MapKey(key), exit, entryStats, now.plus(timeToLive(exit))))
                  promise.done(exit) *> ZIO.done(exit)
                }
                .onInterrupt(promise.interrupt *> ZIO.succeed(map.remove(key)))
            }

            private def newPromise()(implicit unsafe: Unsafe) =
              Promise.unsafe.make[Error, Value](fiberId)

            private def hasExpired(timeToLive: Instant)(implicit unsafe: Unsafe) =
              clock.unsafe.instant().isAfter(timeToLive)
          }
        }
      }
    }

  /**
   * A `MapValue` represents a value in the cache. A value may either be
   * `Pending` with a `Promise` that will contain the result of computing the
   * lookup function, when it is available, or `Complete` with an `Exit` value
   * that contains the result of computing the lookup function.
   */
  private sealed trait MapValue[Key, Error, Value] extends Product with Serializable

  private object MapValue {
    final case class Pending[Key, Error, Value](
      key: MapKey[Key],
      promise: Promise[Error, Value]
    ) extends MapValue[Key, Error, Value]

    final case class Complete[Key, Error, Value](
      key: MapKey[Key],
      exit: Exit[Error, Value],
      entryStats: EntryStats,
      timeToLive: Instant
    ) extends MapValue[Key, Error, Value]

    final case class Refreshing[Key, Error, Value](
      promise: Promise[Error, Value],
      complete: Complete[Key, Error, Value]
    ) extends MapValue[Key, Error, Value]
  }

  /**
   * The `CacheState` represents the mutable state underlying the cache.
   */
  private final case class CacheState[Key, RealKey, Error, Value](
    map: Map[RealKey, MapValue[Key, Error, Value]],
    keys: KeySet[Key],
    accesses: MutableConcurrentQueue[MapKey[Key]],
    hits: LongAdder,
    misses: LongAdder,
    updating: AtomicBoolean
  )

  private object CacheState {

    /**
     * Constructs an initial cache state.
     */
    def initial[Key, RealKey, Error, Value](): CacheState[Key, RealKey, Error, Value] =
      CacheState(
        Platform.newConcurrentMap,
        new KeySet,
        MutableConcurrentQueue.unbounded,
        new LongAdder,
        new LongAdder,
        new AtomicBoolean(false)
      )
  }
}
