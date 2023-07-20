import java.util.concurrent.Executors
import scala.concurrent.{
  ExecutionContext,
  ExecutionContextExecutorService,
  Future
}

/** A higher-kinded type which can compose values (via 'product'), returning a
  * tuple of elements.
  *
  * Although Monad extends Semigroupals, there are some which are useful without
  * being full Monods; for example: Validated.
  *
  * Semigroupals are not to confused with Semigroups.
  */
class SemigroupalSpec extends DemoSpec {

  "Semigroupal" - {
    "can generate the product of two elements, when the type is" - {
      "An Option, when" - {

        import cats.Semigroupal
        import cats.instances.option._ // Enables Semigroupal[Option]

        val semigroupal = Semigroupal[Option]

        "both are 'Some'" in {
          val result: Option[(Int, String)] =
            semigroupal.product(Some(10), Some("Hello"))

          result shouldBe Some(10, "Hello")
        }
        "one is 'None'" in {
          val result: Option[(Int, String)] =
            semigroupal.product(Some(10), Option.empty[String])

          result shouldBe None
        }
        "both are 'None'" in {
          val result: Option[(Int, String)] =
            semigroupal.product(Option.empty[Int], Option.empty[String])

          result shouldBe None
        }
      }
      "a Future" in {
        import cats.Semigroupal
        import cats.instances.future._ // Enables Semigroupal[Future]

        implicit val ec: ExecutionContextExecutorService =
          ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
        val semigroupal = Semigroupal[Future]

        val result: Future[(String, Int)] = semigroupal.product(
          Future.successful("Hello?"),
          Future.successful(10)
        )

        await(result) shouldBe ("Hello?", 10)

      }
      "a List" in {

        import cats.Semigroupal
        import cats.instances.list._ // Enables Semigroupal[List]

        val semigroupal = Semigroupal[List]

        val result = semigroupal.product(List(1, 2), List("a", "b", "c"))

        result shouldBe List(
          (1, "a"),
          (1, "b"),
          (1, "c"),
          (2, "a"),
          (2, "b"),
          (2, "c")
        )

      }
      "a Validated result, when" - {

        import cats.Semigroupal
        import cats.data.Validated
        type ErrorsOr[T] = Validated[List[String], T]
        import cats.instances.list._ // Provides Semigroup[List]
        val semigroupal = Semigroupal[ErrorsOr]

        "both are valid" in {
          val result: ErrorsOr[(Int, String)] =
            semigroupal.product(Validated.valid(10), Validated.valid("a"))

          result shouldBe Validated.valid((10, "a"))
        }
        "only the first is invalid" in {
          val result: ErrorsOr[(String, Int)] = semigroupal.product(
            Validated.invalid(List("Unacceptable!")),
            Validated.valid(10)
          )

          result shouldBe Validated.invalid(List("Unacceptable!"))
        }
        "only the second is invalid" in {
          val result: ErrorsOr[(Int, String)] = semigroupal.product(
            Validated.valid(10),
            Validated.invalid(List("Unacceptable!"))
          )

          result shouldBe Validated.invalid(List("Unacceptable!"))
        }
        "both are invalid" in {
          val result: ErrorsOr[(Int, String)] = semigroupal.product(
            Validated.invalid(List("Simply not good enough.")),
            Validated.invalid(List("Unacceptable!"))
          )

          result shouldBe Validated.invalid(
            List("Simply not good enough.", "Unacceptable!")
          )
        }
      }
      "an Either, when" - {

        import cats.Semigroupal
        import cats.instances.either._

        type EitherErrorsOr[T] = Either[List[String], T]
        val semigroupal = Semigroupal[EitherErrorsOr]

        "both are valid" in {
          val result: EitherErrorsOr[(Int, String)] =
            semigroupal.product(Right(10), Right("OK"))

          result shouldBe Right(10, "OK")
        }
        "only the first is invalid" in {
          val result: EitherErrorsOr[(Int, String)] =
            semigroupal.product(Left(List("Unacceptable!")), Right("OK"))

          result shouldBe Left(List("Unacceptable!"))
        }
        "only the second is invalid" in {
          val result: EitherErrorsOr[(Int, String)] = semigroupal.product(
            Right(10),
            Left(List("Simply not good enough."))
          )

          result shouldBe Left(List("Simply not good enough."))
        }
        "both are invalid" in {
          val result: EitherErrorsOr[(Int, String)] = semigroupal.product(
            Left(List("Unacceptable!")),
            Left(List("Simply not good enough."))
          )

          result shouldBe Left(
            List("Unacceptable!")
          ) // Note that it short circuits
        }
      }
    }
  }
}
