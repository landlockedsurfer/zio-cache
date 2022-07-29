package zio.cache.app

import zio._
import zio.cache._

case class CookieData(data: Int)
case class Request(userId: Int, cookieData: CookieData)

trait UserSessionDataComputationService {
  type UserSessionData = UserSessionDataCacheValue
  def expensiveUserSessionDataComputation(key: Int, request: Request): UIO[UserSessionData]
}
object UserSessionDataComputationService {
  val live = ZLayer.succeed(
    new UserSessionDataComputationService {
      override def expensiveUserSessionDataComputation(key: Int, request: Request): UIO[UserSessionDataCacheValue] =
        ZIO.succeed(UserSessionDataCacheValue(request.cookieData.data))
    }
  )
}

case class UserSessionDataCacheValue(result: Int)

object CacheApp extends zio.ZIOAppDefault {

  val layer
    : ZLayer[UserSessionDataComputationService, Nothing, Cache[Int, Request, Nothing, UserSessionDataCacheValue]] =
    ZLayer.service[UserSessionDataComputationService].flatMap { userSessionDataComputationService =>
      ZLayer.fromZIO(
        zio.cache.Cache.make[Int, Request, Nothing, UserSessionDataCacheValue](
          1000,
          100.seconds,
          Lookup { key =>
            ZIO.service[Request].flatMap { r =>
              userSessionDataComputationService.get.expensiveUserSessionDataComputation(key, r)
            }
          }
        )
      )
    }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    (for {
      cache <- ZIO.service[zio.cache.Cache[Int, Request, Nothing, UserSessionDataCacheValue]]
      r     <- cache.get(2).provideEnvironment(ZEnvironment(Request(2, CookieData(1))))
      _     <- ZIO.logInfo(r.toString)
      _     <- cache.invalidate(2)
      r     <- cache.get(2).provideEnvironment(ZEnvironment(Request(2, CookieData(42))))
      _     <- ZIO.logInfo(r.toString)
    } yield {
      ()
    }).provide(layer, UserSessionDataComputationService.live)
}
