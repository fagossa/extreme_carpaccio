import com.typesafe.config.ConfigRenderOptions
import play.api.http.HeaderNames._
import play.api.mvc.{Filter, RequestHeader, Result, WithFilters}
import play.api.{Application, GlobalSettings}

import scala.concurrent.Future

object Global extends WithFilters(NoCacheFilter) with GlobalSettings {

  override def onStart(app: Application): Unit = {
    val renderer = ConfigRenderOptions.defaults().setComments(false).setOriginComments(false)
    println( s"""
        |
        |
        |Application configuration
        |
        |${play.api.Play.current.configuration.underlying.root().render(renderer)}
        |
        |
        |""".stripMargin)
  }

}

object NoCacheFilter extends Filter {
  override def apply(nextFilter: (RequestHeader) => Future[Result])(requestHeader: RequestHeader): Future[Result] = {
    import play.api.libs.concurrent.Execution.Implicits._
    nextFilter(requestHeader).map(_.withHeaders(CACHE_CONTROL -> "no-cache", PRAGMA -> "no-cache"))
  }
}
