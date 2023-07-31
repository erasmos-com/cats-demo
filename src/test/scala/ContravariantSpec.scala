/** Contravariant type classes have nothing to do with the contravariance of
  * types.
  *
  * Whereas map applies transformations in written sequence, contramap applies
  * such transformations in reverse.
  */
class ContravariantSpec extends DemoSpec {

  "'contramap' applies transformations in reverse order; for example" - {
    "using Show, wrapping in an" - {
      "Option" - {
        "explicitly" in {
          import cats.instances.int._
          import cats.{Contravariant, Show}
          val innerShow: Show[Int] = Show[Int]
          val outerShow: Show[Option[Int]] =
            Contravariant[Show].contramap[Int, Option[Int]](innerShow)(
              _.getOrElse(0)
            )

          val result = outerShow.show(Some(10))

          result shouldBe "10"
        }
        "with the extension method" in {
          import cats.Show
          import cats.instances.int._
          import cats.syntax.contravariant._
          val innerShow: Show[Int] = Show[Int]
          val outerShow: Show[Option[Int]] =
            innerShow.contramap[Option[Int]](_.getOrElse(0))

          val result: String = outerShow.show(None)

          result shouldBe "0"
        }
      }
      "a Try" - {
        "explicitly" in {
          import cats.instances.int._
          import cats.{Contravariant, Show}
          import scala.util.{Failure, Success, Try}
          val innerShow = Show[Int]
          val outerShow: Show[Try[Int]] =
            Contravariant[Show].contramap[Int, Try[Int]](innerShow) {
              case Success(value) => value
              case Failure(_)     => 0
            }

          val result: String = outerShow.show(Success(10))

          result shouldBe "10"
        }
        "with the extension method" in {
          import cats.Show
          import cats.instances.int._
          import cats.syntax.contravariant._
          import scala.util.{Failure, Success, Try}
          val innerShow = Show[Int]
          val outerShow: Show[Try[Int]] = innerShow.contramap[Try[Int]] {
            case Success(value) => value
            case Failure(_)     => 0
          }

          val result: String = outerShow.show(Success(10))

          result shouldBe "10"
        }
      }
    }
  }
}
