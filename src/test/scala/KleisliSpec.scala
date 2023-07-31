/** Allows the wrapping of functions, returning F[_] instances.
  *
  * Supports function composition, and well as providing convenience functions
  * like: map, flatMap, apply, andThen, traverse etc ...
  *
  * Reader is a type of Kleisi where the wrapper is 'identity'.
  */
class KleisliSpec extends DemoSpec {

  "a Kleisi can" - {

    def isEven(x: Int): Option[String] =
      if (x % 2 == 0) Some(x.toString) else None

    def tripleItIfPositive(x: Int): Option[Int] =
      if (x > 0) Some(x * 3) else None

    "wrap a function; for instance, in" - {
      "an Option, where that function" - {
        "returns a string" - {
          "expecting Some" in {
            import cats.data.Kleisli
            val kleisli: Kleisli[Option, Int, String] = Kleisli(isEven)

            val result: Option[String] = kleisli.run(10)

            result shouldBe Some("10")
          }
          "expecting None" in {
            import cats.data.Kleisli
            val kleisli: Kleisli[Option, Int, String] = Kleisli(isEven)

            val result: Option[String] = kleisli.run(9)

            result shouldBe None
          }
        }
        "returns an int" - {
          "expecting Some" in {
            import cats.data.Kleisli
            val kleisli: Kleisli[Option, Int, Int] = Kleisli(tripleItIfPositive)

            val result = kleisli.run(5)

            result shouldBe Some(15)
          }
          "expecting None" in {
            import cats.data.Kleisli
            val kleisli: Kleisli[Option, Int, Int] = Kleisli(tripleItIfPositive)

            val result = kleisli.run(-4)

            result shouldBe None
          }
        }
        "composes the two" - {
          "expecting Some" in {
            import cats.data.Kleisli
            import cats.instances.option._ // For FlatMap[Option]
            val kleisli: Kleisli[Option, Int, String] =
              Kleisli(tripleItIfPositive) andThen Kleisli(isEven)

            val result = kleisli.run(10)

            result shouldBe Some("30")
          }
          "expecting None" in {
            import cats.data.Kleisli
            import cats.instances.option._ // For FlatMap[Option]
            val kleisli: Kleisli[Option, Int, String] =
              Kleisli(tripleItIfPositive) andThen Kleisli(isEven)

            val result = kleisli.run(3)

            result shouldBe None
          }
        }
      }
    }
    "transform itself, via" - {
      "map" in {
        import cats.data.Kleisli
        import cats.instances.option._ // For Functor[Option] which provides map()
        val kleisi = Kleisli(isEven).map("[" + _ + "]")

        val result: Option[String] = kleisi(10)

        result shouldBe Some("[10]")

      }
      "flatMap" - {
        "explicitly" in {
          import cats.Id
          import cats.data.{Kleisli, Reader}
          val double: Kleisli[Id, Int, Int]  = Reader[Int, Int](x => x * 2)
          val addFour: Kleisli[Id, Int, Int] = Reader[Int, Int](x => x + 4)
          val kleisli = double.flatMap(doubled =>
            addFour.map(fourAdded => doubled + fourAdded)
          )

          val result = kleisli(10)

          result shouldBe 34
        }
        "with for comprehension" in {
          import cats.Id
          import cats.data.{Kleisli, Reader}
          val double: Kleisli[Id, Int, Int]  = Reader[Int, Int](x => x * 2)
          val addFour: Kleisli[Id, Int, Int] = Reader[Int, Int](x => x + 4)
          val kleisli = for {
            doubled   <- double
            fourAdded <- addFour
          } yield doubled + fourAdded

          val result = kleisli(10)

          result shouldBe 34
        }
      }
    }
  }
}
