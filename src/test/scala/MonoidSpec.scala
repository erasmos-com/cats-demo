import MonoidSpec.{Dimensions, dimensionsMonoid, genericFold}

/** A monoid extends a Semigroup by providing a starting (or "empty") value,
  * making is suitable for use with a fold, for example.
  */
class MonoidSpec extends DemoSpec {

  "Monoids apply to different types, such as" - {
    "an int" - {
      "where it provides" - {
        "an empty value" in {
          import cats.Monoid
          import cats.instances.int._
          val monoid = Monoid[Int]

          monoid.empty shouldBe 0
        }
        "a 'combine' function" in {
          import cats.Monoid
          import cats.instances.int._
          val monoid = Monoid[Int]

          monoid.combine(10, 32) shouldBe 42
        }
        "a 'combine' function (wrapped in an Option)" in {
          import cats.Monoid
          import cats.instances.int._
          import cats.instances.option._
          val monoid = Monoid[Option[Int]]

          monoid.combine(Option(42), Option.empty[Int]) shouldBe Some(42)
        }
        "the means for a fold, using" - {
          "the 'combine' function" in {
            import cats.Monoid
            import cats.instances.int._
            val monoid   = Monoid[Int]
            val sequence = Seq(2, 4, 6, 8, 10)

            sequence.fold(monoid.empty)(monoid.combine) shouldBe 30
          }
          "syntax from semigroup" in {
            import cats.Monoid
            import cats.instances.int._
            import cats.syntax.semigroup._
            val monoid   = Monoid[Int]
            val sequence = Seq(2, 4, 6, 8, 10)

            sequence.fold(monoid.empty)(_ |+| _) shouldBe 30
          }
          "our generic function" in {
            import cats.instances.int._
            val sequence = Seq(2, 4, 6, 8, 10)

            genericFold(sequence) shouldBe 30
          }

        }
      }
    }
    "a string" - {
      "where it provides" - {
        "an empty value" in {
          import cats.Monoid
          import cats.instances.string._
          val monoid = Monoid[String]

          monoid.empty shouldBe ""
        }
        "a 'combine' function" in {
          import cats.Monoid
          import cats.instances.string._
          val monoid = Monoid[String]

          monoid.combine("Hello, ", "World!") shouldBe "Hello, World!"
        }
        "a 'combine' function (wrapped in an Option)" in {
          import cats.Monoid
          import cats.instances.option._
          import cats.instances.string._
          val monoid = Monoid[Option[String]]

          monoid.combine(
            Option("Hello, World!"),
            Option.empty[String]
          ) shouldBe Some("Hello, World!")
        }
        "the means for a fold, using" - {
          "the 'combine' function" in {
            import cats.Monoid
            import cats.instances.string._
            val monoid   = Monoid[String]
            val sequence = Seq("Hello", ",", " ", "World", "!")

            sequence.fold(monoid.empty)(monoid.combine) shouldBe "Hello, World!"
          }
          "syntax from semigroup" in {
            import cats.Monoid
            import cats.instances.string._
            import cats.syntax.semigroup._
            val monoid   = Monoid[String]
            val sequence = Seq("Hello", ",", " ", "World", "!")

            sequence.fold(monoid.empty)(_ |+| _) shouldBe "Hello, World!"
          }
          "our generic function" in {
            import cats.instances.string._
            val sequence = Seq("Hello", ",", " ", "World", "!")

            genericFold(sequence) shouldBe "Hello, World!"
          }
        }

      }
    }
    "a custom type" - {
      "where it provides" - {
        "an empty value" in {
          dimensionsMonoid.empty shouldBe Dimensions(0, 0)
        }
        "a 'combine' function" in {
          val first  = Dimensions(18, 80)
          val second = Dimensions(16, 100)

          val result = dimensionsMonoid.combine(first, second)

          result shouldBe Dimensions(34, 180)
        }
        "a 'combine' function, using syntax (wrapped in an Option)" in {
          import cats.instances.option._
          import cats.syntax.semigroup._
          val first  = Option(Dimensions(18, 80))
          val second = Option(Dimensions(16, 100))

          val result = first |+| second

          result shouldBe Some(Dimensions(34, 180))
        }
        "the means for a fold, using" - {
          "the 'combine' function" in {
            val sequence =
              Seq(Dimensions(18, 80), Dimensions(12, 100), Dimensions(12, 20))

            sequence.fold(dimensionsMonoid.empty)(
              dimensionsMonoid.combine
            ) shouldBe Dimensions(42, 200)
          }
          "syntax from semigroup" in {
            import cats.syntax.semigroup._
            val sequence =
              Seq(Dimensions(18, 80), Dimensions(12, 100), Dimensions(12, 20))

            sequence.fold(dimensionsMonoid.empty)(
              _ |+| _
            ) shouldBe Dimensions(42, 200)
          }
          "our generic function" in {
            val sequence =
              Seq(Dimensions(18, 80), Dimensions(12, 100), Dimensions(12, 20))

            genericFold(sequence) shouldBe Dimensions(42, 200)
          }
        }

      }
    }

  }
}

object MonoidSpec {
  import cats.Monoid

  def genericFold[T](sequence: Seq[T])(implicit monoid: Monoid[T]): T = {
    import cats.syntax.semigroup._
    sequence.foldLeft(monoid.empty)(_ |+| _)
  }

  case class Dimensions(width: Int, length: Int)

  implicit val dimensionsMonoid: Monoid[Dimensions] =
    Monoid.instance[Dimensions](
      Dimensions(0, 0),
      { (first, second) =>
        Dimensions(first.width + second.width, first.length + second.length)
      }
    )

}
