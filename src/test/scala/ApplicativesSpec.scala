/** Applicatives offer the 'pure' function.
  *
  * They inherit 'map' from Functor.
  *
  * They are extended by Monad
  */
class ApplicativesSpec extends DemoSpec {

  "Applicatives" - {
    "provide a 'pure' function for various types including" - {
      "Option, using" - {

        import cats.Applicative
        import cats.instances.option._

        val applicative = Applicative[Option]

        "the static method" in {
          val result: Option[Int] = applicative.pure(4)

          result shouldBe Some(4)
        }
        "the extension method" in {
          import cats.syntax.applicative._
          val result = 4.pure

          result shouldBe Some(4)
        }
      }
      "List, using" - {

        import cats.Applicative
        import cats.instances.list._

        val applicative = Applicative[List]

        "the static method" in {
          val result: List[Int] = applicative.pure(4)

          result shouldBe List(4)
        }
        "the extension method" in {
          import cats.syntax.applicative._
          val result: List[Int] = 4.pure

          result shouldBe List(4)
        }
      }
      "Validated, using" - {

        import cats.data.Validated
        import cats.Applicative
        import cats.instances.list._

        type ErrorsOr[T] = Validated[List[String], T]
        val applicative = Applicative[ErrorsOr]

        "the static method" in {
          val result: ErrorsOr[Int] = applicative.pure(4)

          result shouldBe Validated.Valid(4)
        }
        "the extension method" in {
          import cats.syntax.applicative._

          val result: ErrorsOr[Int] =
            4.pure[ErrorsOr] // Otherwise assumed to be an Applicative[List]

          result shouldBe Validated.Valid(4)
        }
      }
    }
  }
}
