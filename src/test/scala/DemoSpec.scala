import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

/** General Notes:
  *
  * Throughout we use 'eqv' instead of '===' from Cats, as '===' is provided by
  * Scalatic via Scalatest.
  */
trait DemoSpec extends AnyFreeSpecLike with Matchers {}
