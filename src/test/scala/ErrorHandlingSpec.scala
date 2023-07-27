class ErrorHandlingSpec extends DemoSpec {

  "A MonadError when" - {
    "typed as an Either / String can" - {

      type ErrorOr[A] = Either[String, A]

      "be successful" in {
        import cats.MonadError
        import cats.instances.either._
        val monadError = MonadError[ErrorOr, String]

        val result = monadError.pure(42)

        result shouldBe Right(42)
      }
      "raise an error" in {
        import cats.MonadError
        import cats.instances.either._
        val monadError = MonadError[ErrorOr, String]

        val result = monadError.raiseError("Something went wrong")

        result shouldBe Left("Something went wrong")
      }
      "handle an error" in {
        import cats.MonadError
        import cats.instances.either._
        val monadError     = MonadError[ErrorOr, String]
        val failureHandler = monadError.raiseError[Int]("Something went wrong")

        val result = monadError.handleError(failureHandler) {
          case "Something went wrong" => 42
          case _                      => 0
        }

        result shouldBe Right(42)
      }
      "handle an error (returning wrapped values)" in {
        import cats.MonadError
        import cats.instances.either._
        val monadError     = MonadError[ErrorOr, String]
        val failureHandler = monadError.raiseError[Int]("Something went wrong")

        val result = monadError.handleErrorWith(failureHandler) {
          case "Something went wrong" => monadError.pure(42)
          case _                      => monadError.pure(0)
        }

        result shouldBe Right(42)
      }
      "recover from an error" in {
        import cats.MonadError
        import cats.instances.either._
        import cats.syntax.either._
        val monadError     = MonadError[ErrorOr, String]
        val failureHandler = monadError.raiseError("Something went wrong")

        val result = failureHandler.recover {
          case "Something went wrong" => 0
          case _                      => -1
        }

        result shouldBe Right(0)
      }
      "ensure a condition when" - {
        "explicitly" - {
          "met" in {
            import cats.MonadError
            import cats.instances.either._
            val monadError = MonadError[ErrorOr, String]
            val success    = monadError.pure(42)

            val result = monadError.ensure(success)("Number too small")(_ < 100)

            result shouldBe Right(42)
          }
          "unmet" in {
            import cats.MonadError
            import cats.instances.either._
            val monadError = MonadError[ErrorOr, String]
            val success    = monadError.pure(42)

            val result = monadError.ensure(success)("Number too small")(_ < 40)

            result shouldBe Left("Number too small")
          }

        }
        "with extension method" - {
          "met" in {
            import cats.MonadError
            import cats.instances.either._
            import cats.syntax.monadError._
            val monadError = MonadError[ErrorOr, String]
            val success    = monadError.pure(42)

            val result = success.ensure("Number too small")(_ < 100)

            result shouldBe Right(42)
          }
          "unmet" in {
            import cats.MonadError
            import cats.instances.either._
            import cats.syntax.monadError._
            val monadError = MonadError[ErrorOr, String]
            val success    = monadError.pure(42)

            val result = success.ensure("Number too small")(_ < 40)

            result shouldBe Left("Number too small")
          }
        }
      }
    }
    "typed as a Try / Throwable can" - {
      "be successful" in {
        import cats.MonadError
        import cats.instances.try_._
        import scala.util.{Success, Try}
        val monadError = MonadError[Try, Throwable]

        val result = monadError.pure(42)

        result shouldBe Success(42)
      }
      "raise an error" in {
        import cats.MonadError
        import cats.instances.try_._
        import scala.util.{Failure, Try}
        val monadError = MonadError[Try, Throwable]
        val exception  = new RuntimeException("!")

        val result = monadError.raiseError(exception)

        result shouldBe Failure(exception)
      }
      "handle an error" in {
        import cats.MonadError
        import cats.instances.try_._
        import scala.util.{Success, Try}
        val monadError     = MonadError[Try, Throwable]
        val exception      = new RuntimeException("!")
        val failureHandler = monadError.raiseError(exception)

        val result = monadError.handleError(failureHandler) {
          case e if e.getMessage == "!" => 42
          case _                        => 0
        }

        result shouldBe Success(42)
      }
      "handle an error (returning wrapped values)" in {
        import cats.MonadError
        import cats.instances.try_._
        import scala.util.{Failure, Success, Try}
        val monadError       = MonadError[Try, Throwable]
        val exception        = new RuntimeException("?")
        val anotherException = new RuntimeException("Failure!")
        val failureHandler   = monadError.raiseError(exception)

        val result = monadError.handleErrorWith(failureHandler) {
          case e if e.getMessage == "!" => Success(42)
          case _                        => Failure(anotherException)
        }

        result shouldBe Failure(anotherException)
      }
      "recover from an error" in {
        import cats.MonadError
        import cats.instances.try_._
        import scala.util.{Success, Try}
        val monadError     = MonadError[Try, Throwable]
        val exception      = new RuntimeException("!")
        val failureHandler = monadError.raiseError(exception)

        val result = failureHandler.recover {
          case e if e.getMessage == "!" => 42
          case _                        => 0
        }

        result shouldBe Success(42)
      }
      "ensure a condition" - {
        "explicitly" - {
          "met" in {
            import cats.MonadError
            import cats.instances.try_._
            import scala.util.{Success, Try}
            val monadError = MonadError[Try, Throwable]
            val success    = monadError.pure(42)
            val exception  = new RuntimeException("?")

            val result = monadError.ensure(success)(exception)(_ < 100)

            result shouldBe Success(42)
          }
          "unmet" in {
            import cats.MonadError
            import cats.instances.try_._
            import scala.util.{Failure, Try}

            val monadError = MonadError[Try, Throwable]
            val success    = monadError.pure(42)
            val exception  = new RuntimeException("?")

            val result = monadError.ensure(success)(exception)(_ < 40)

            result shouldBe Failure(exception)
          }
        }
        "with extension method" - {
          "met" in {
            import cats.MonadError
            import cats.instances.try_._
            import cats.syntax.monadError._
            import scala.util.{Success, Try}

            val monadError = MonadError[Try, Throwable]
            val success    = monadError.pure(42)
            val exception  = new RuntimeException("!")

            val result = success.ensure(exception)(_ < 100)

            result shouldBe Success(42)
          }
          "unmet" in {
            import cats.MonadError
            import cats.instances.try_._
            import cats.syntax.monadError._
            import scala.util.{Failure, Try}

            val monadError = MonadError[Try, Throwable]
            val success    = monadError.pure(42)
            val exception  = new RuntimeException("!")

            val result = success.ensure(exception)(_ < 40)

            result shouldBe Failure(exception)
          }
        }
      }
    }
  }
}
