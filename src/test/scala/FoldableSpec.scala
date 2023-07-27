import scala.annotation.unused

/** A higher-kinded type class (aka a container)
  *
  * Folding right is stack-safe, regardless of container, as it requires an Eval
  * for the accumulator.
  *
  * Also useful for combining all.
  */
class FoldableSpec extends DemoSpec {

  "Folding is applicable to various higher-kinded types (containers), like" - {
    "an Option" - {
      "and offers the ability to" - {
        "fold left" in {
          import cats.Foldable
          import cats.instances.option._
          val foldable = Foldable[Option]

          val result: Int = foldable.foldLeft(Option(2), 40)(_ + _)

          result shouldBe 42
        }
        "fold right" in {
          import cats.instances.option._
          import cats.{Eval, Foldable}
          val foldable = Foldable[Option]

          val result: Eval[Int] = foldable.foldRight(Option(2), Eval.now(40)) {
            (number: Int, eval: Eval[Int]) =>
              eval.map(_ + number)
          }

          result.value shouldBe 42
        }
        "fold map" in {
          import cats.instances.option._
          import cats.instances.int._
          import cats.Foldable
          val foldable = Foldable[Option]

          val result: Int = foldable.foldMap(Option(2))(_ + 40)

          result shouldBe 42
        }
        "combine all" in {
          import cats.Foldable
          import cats.instances.int._
          import cats.instances.option._
          val foldable = Foldable[Option]

          val result: Int = foldable.combineAll(Option(2))

          result shouldBe 2
        }
      }
    }
    "a List" - {
      "and offers the ability to" - {
        "fold left" in {
          import cats.Foldable
          import cats.instances.list._
          val foldable = Foldable[List]

          val result: Int = foldable.foldLeft(List(1, 2, 4, 8), 5)(_ + _)

          result shouldBe 20
        }
        "fold right" in {
          import cats.instances.list._
          import cats.{Eval, Foldable}
          val foldable = Foldable[List]

          val result: Eval[Int] =
            foldable.foldRight(List(1, 2, 4, 8), Eval.now(5)) {
              (number: Int, eval: Eval[Int]) =>
                {
                  eval.map(_ + number)
                }
            }

          result.value shouldBe 20
        }
        "fold map" in {
          import cats.Foldable
          import cats.instances.list._
          import cats.instances.string._
          val foldable = Foldable[List]

          val result: String = foldable.foldMap(List(1, 2, 4, 8))(_.toString)

          result shouldBe "1248"
        }
        "combine all" - {
          "explicitly" in {
            import cats.Foldable
            import cats.instances.int._
            import cats.instances.list._
            val foldable = Foldable[List]

            val result: Int = foldable.combineAll(List(1, 2, 4, 8))

            result shouldBe 15
          }
          "using extension method" in {
            import cats.Monoid
            import cats.instances.int._
            import cats.instances.list._
            import cats.syntax.foldable._
            @unused // The import of cats.instances.int._ is claimed to be unused
            implicit val monoid = Monoid[Int]

            val result: Int = List(1, 2, 4, 8).combineAll

            result shouldBe 15
          }
        }

      }
    }
    "a nested List" - {
      "and offers the ability to" - {
        "fold left" in {
          import cats.Foldable
          import cats.instances.list._
          import cats.instances.vector._
          val foldable = Foldable[List] compose Foldable[Vector]

          val result: Int =
            foldable.foldLeft(List(Vector(1, 2, 3), Vector(4, 5, 6)), 10)(_ + _)

          result shouldBe 31
        }
        "fold right" in {
          import cats.instances.list._
          import cats.instances.vector._
          import cats.{Eval, Foldable}
          val foldable = Foldable[List] compose Foldable[Vector]

          val result: Eval[Int] = foldable.foldRight(
            List(Vector(1, 2, 3), Vector(4, 5, 6)),
            Eval.now(10)
          ) { (number: Int, eval: Eval[Int]) =>
            eval.map(_ + number)
          }

          result.value shouldBe 31
        }
        "fold map" in {
          import cats.Foldable
          import cats.instances.list._
          import cats.instances.string._
          import cats.instances.vector._
          val foldable = Foldable[List] compose Foldable[Vector]

          val result: String =
            foldable.foldMap(List(Vector(1, 2, 3), Vector(4, 5, 6)))(_.toString)

          result shouldBe "123456"
        }
        "combine all" - {
          "explicitly" in {
            import cats.Foldable
            import cats.instances.int._
            import cats.instances.list._
            import cats.instances.vector._
            val foldable = Foldable[List] compose Foldable[Vector]

            val result: Int =
              foldable.combineAll(List(Vector(1, 2, 3), Vector(4, 5, 6)))

            result shouldBe 21
          }
          "using extension method" in {
            import cats.Monoid
            import cats.instances.list._
            import cats.instances.vector._
            import cats.instances.int._
            import cats.syntax.foldable._
            @unused // Merely importing cats.instances doesn't work, as it's seen to be unused.
            implicit val monoid = Monoid[Vector[Int]]

            val result: Vector[Int] =
              List(Vector(1, 2, 3), Vector(4, 5, 6)).combineAll

            result shouldBe Vector(1, 2, 3, 4, 5, 6)
          }
        }
      }
    }
  }

}
