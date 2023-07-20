class ValidationSpec extends DemoSpec {

  "Validation" - {
    "results in one of two states" - {
      "'Valid', using" - {
        "using static method" in {
          import cats.data.Validated
          import cats.data.Validated.Valid

          val result: Validated[String, Int] = Validated.valid(42)

          result shouldBe Valid(42)
        }
        "using extension method" in {
          import cats.data.Validated
          import cats.data.Validated.Valid
          import cats.syntax.validated._

          val result: Validated[String, Int] = 42.valid[String]

          result shouldBe Valid(42)
        }
      }
      "'Invalid'" - {
        "using static method" in {
          import cats.data.Validated
          import cats.data.Validated.Invalid

          val result: Validated[String, Int] =
            Validated.invalid("Not at all acceptable")

          result shouldBe Invalid("Not at all acceptable")
        }
        "using extension method" in {
          import cats.data.Validated
          import cats.data.Validated.Invalid
          import cats.syntax.validated._

          val result: Validated[String, Int] =
            "Not at all acceptable".invalid[Int]

          result shouldBe Invalid("Not at all acceptable")
        }
      }
    }
    "can be based on a predicate" - {
      "using 'cond', when" - {
        "true" in {
          import cats.data.Validated
          import cats.data.Validated.Valid

          val number = 22
          val result =
            Validated.cond(
              number % 2 == 0,
              1,
              "Number is too odd for my tastes"
            )

          result shouldBe Valid(1)
        }
        "false" in {
          import cats.data.Validated
          import cats.data.Validated.Invalid

          val number = 21
          val result =
            Validated.cond(
              number % 2 == 0,
              1,
              "Number is too odd for my tastes"
            )

          result shouldBe Invalid("Number is too odd for my tastes")
        }
      }
      "using 'ensure', when" - {
        "true" in {
          import cats.data.Validated.Valid

          val number = 22

          val result =
            Valid(number).ensure("Number is too odd for my tastes")(_ % 2 == 0)

          result shouldBe Valid(22)
        }
        "false" in {

          import cats.data.Validated.{Invalid, Valid}

          val number = 21

          val result =
            Valid(number).ensure("Number is too odd for my tastes")(_ % 2 == 0)

          result shouldBe Invalid("Number is too odd for my tastes")
        }
      }
    }
    "can be composed" - {
      "when validation" - {
        import cats.data.Validated
        import cats.instances.list._
        import cats.kernel.Semigroup // Brings in the Semigroup for List[String]

        implicit val semigroupForCombiningInts: Semigroup[Int] =
          Semigroup.instance[Int]((first, _) => first)
        def validate(x: Int): Validated[List[String], Int] =
          Validated
            .cond(x % 2 == 0, x, List("Number must be even"))
            .combine(
              Validated
                .cond(x >= 0, x, List("Number must be positive"))
                .combine(
                  Validated.cond(
                    x <= 100,
                    x,
                    List("Number must be less than or equal to 100")
                  )
                )
            )

        "fails for" - {
          "first condition, only" in {
            import cats.data.Validated.Invalid

            val result: Validated[List[String], Int] = validate(11)

            result shouldBe Invalid(List("Number must be even"))
          }
          "second condition, only" in {
            import cats.data.Validated.Invalid

            val result: Validated[List[String], Int] = validate(-2)

            result shouldBe Invalid(List("Number must be positive"))
          }
          "third condition, only" in {
            import cats.data.Validated.Invalid

            val result: Validated[List[String], Int] = validate(102)

            result shouldBe Invalid(
              List("Number must be less than or equal to 100")
            )
          }
          "two conditions" in {
            import cats.data.Validated.Invalid

            val result: Validated[List[String], Int] = validate(101)

            result shouldBe Invalid(
              List(
                "Number must be even",
                "Number must be less than or equal to 100"
              )
            )
          }
        }
        "succeeds" in {
          import cats.data.Validated.Valid

          val result: Validated[List[String], Int] = validate(42)

          result shouldBe Valid(42)
        }
      }
    }
    "can be converted from" - {
      "an Either, when" - {
        "Valid" in {
          import cats.data.Validated
          import cats.data.Validated.Valid

          val result: Validated[String, Int] =
            Validated.fromEither[String, Int](Right(10))

          result shouldBe Valid(10)
        }
        "Invalid" in {
          import cats.data.Validated
          import cats.data.Validated.Invalid

          val result: Validated[String, Int] =
            Validated.fromEither[String, Int](Left("Unacceptable!"))

          result shouldBe Invalid("Unacceptable!")
        }
      }
      "an Option, when" - {
        "Valid" in {
          import cats.data.Validated
          import cats.data.Validated.Valid

          val result: Validated[String, Int] =
            Validated.fromOption(Some(10), "Unacceptable!")

          result shouldBe Valid(10)
        }
        "Invalid" in {
          import cats.data.Validated
          import cats.data.Validated.Invalid

          val result: Validated[String, Int] =
            Validated.fromOption(None, "Unacceptable!")

          result shouldBe Invalid("Unacceptable!")
        }
      }
      "a Try, when" - {
        "Valid" in {
          import cats.data.Validated
          import cats.data.Validated.Valid
          import scala.util.Success

          val result: Validated[Throwable, Int] = Validated.fromTry(Success(10))

          result shouldBe Valid(10)
        }
        "Invalid" in {
          import cats.data.Validated
          import cats.data.Validated.Invalid
          import scala.util.Failure

          val exception = new RuntimeException("Unacceptable!")
          val result: Validated[Throwable, Int] =
            Validated.fromTry(Failure(exception))

          result shouldBe Invalid(exception)
        }
      }
    }
    "can be transformed, using" - {
      "using 'map', when" - {
        "valid" in {
          import cats.data.Validated
          import cats.data.Validated.Valid

          val result: Validated[String, Int] =
            Validated.valid[String, Int](10).map(_ + 2)

          result shouldBe Valid(12)
        }
        "invalid" in {
          import cats.data.Validated
          import cats.data.Validated.Invalid

          val result: Validated[String, Int] =
            Validated.invalid[String, Int]("Unacceptable!").map(_ + 2)

          result shouldBe Invalid("Unacceptable!")
        }
      }
      "using 'leftMap', when" - {
        "valid" in {
          import cats.data.Validated
          import cats.data.Validated.Valid

          val result: Validated[Int, Int] =
            Validated.valid[String, Int](10).leftMap(_.length)

          result shouldBe Valid(10)
        }
        "invalid" in {
          import cats.data.Validated
          import cats.data.Validated.Invalid

          val result: Validated[String, Int] = Validated
            .invalid[String, Int]("Unacceptable!")
            .leftMap(_.toUpperCase)

          result shouldBe Invalid("UNACCEPTABLE!")
        }
      }
      "using 'biMap', when" - {
        "valid" in {
          import cats.data.Validated
          import cats.data.Validated.Valid

          val result: Validated[String, Int] =
            Validated.valid[String, Int](10).bimap(_.toUpperCase, _ + 2)

          result shouldBe Valid(12)
        }
        "invalid" in {
          import cats.data.Validated
          import cats.data.Validated.Invalid

          val result: Validated[String, Int] = Validated
            .invalid[String, Int]("Unacceptable!")
            .bimap(_.toUpperCase, _ + 2)

          result shouldBe Invalid("UNACCEPTABLE!")
        }
      }
    }
  }
}
