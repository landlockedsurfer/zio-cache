package zio.cache.app

import zio._
import zio.cache._

case class CookieData(data: Int)
case class Request(userId: Int, cookieData: CookieData)

trait UserSessionDataComputationService {
  type UserSessionData = UserSessionDataCacheValue
  def expensiveUserSessionDataComputation(request: Request): UIO[UserSessionData]
}
object UserSessionDataComputationService {
  val live = ZLayer.succeed(
    new UserSessionDataComputationService {
      override def expensiveUserSessionDataComputation(request: Request): UIO[UserSessionDataCacheValue] =
        ZIO.succeed(UserSessionDataCacheValue(request.cookieData.data))
    }
  )
}

case class UserSessionDataCacheValue(result: Int)

object DemoApp extends zio.ZIOAppDefault {

  val layer: ZLayer[UserSessionDataComputationService, Nothing, Cache[Request, Nothing, UserSessionDataCacheValue]] =
    ZLayer.service[UserSessionDataComputationService].flatMap { userSessionDataComputationService =>
      ZLayer.fromZIO(
        zio.cache.Cache.makeWith[Request, Int, UserSessionDataComputationService, Nothing, UserSessionDataCacheValue](
          1000,
          Lookup { request: Request =>
            userSessionDataComputationService.get.expensiveUserSessionDataComputation(request)
          },
          _.userId
        )(- => 5.seconds)
      )
    }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    (for {
      cache <- ZIO.service[zio.cache.Cache[Request, Nothing, UserSessionDataCacheValue]]
      r     <- cache.get(Request(2, CookieData(2)))
      _     <- ZIO.logInfo(r.toString)
      _     <- ZIO.sleep(6.seconds)
      r     <- cache.get(Request(2, CookieData(4)))
      _     <- ZIO.logInfo(r.toString)
    } yield {
      ()
    }).provide(layer, UserSessionDataComputationService.live)
}
