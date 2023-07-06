import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/** General Notes:
  *
  * Throughout we use 'eqv' instead of '===' from Cats, as '===' is provided by
  * Scalatic via Scalatest.
  */
trait DemoSpec extends AnyFreeSpecLike with Matchers {

  def await[T](f: Future[T]): T =
    Await.result(f, Duration(100, TimeUnit.MILLISECONDS))

}
