/** Semigroups are used to combine elements of the same type.
  */
class SemigroupSpec extends DemoSpec {

  "semigroups apply to different types, both" - {
    "directly; for example with" - {
      "int" - {
        "without extension method" in {
          import cats.Semigroup
          import cats.instances.int._
          val semigroup = Semigroup[Int]

          semigroup.combine(2, 40) shouldBe 42
        }
        "with extension method" in {
          import cats.instances.int._
          import cats.syntax.semigroup._

          2 |+| 40 shouldBe 42
        }
      }
      "string" - {
        "without extension method" in {
          import cats.Semigroup
          import cats.instances.string._
          val semigroup = Semigroup[String]

          semigroup.combine("Hello,", " World!") shouldBe "Hello, World!"
        }
        "with extension method" in {
          import cats.instances.string._
          import cats.syntax.semigroup._

          "Hello," |+| " World!" shouldBe "Hello, World!"
        }
      }
      "custom type" - {
        "without extension method" in {
          import cats.Semigroup
          case class Walk(stepsTaken: Int)
          val semigroup = new Semigroup[Walk] {
            override def combine(first: Walk, second: Walk): Walk =
              Walk(first.stepsTaken + second.stepsTaken)
          }

          semigroup.combine(Walk(2_000), Walk(4_000)) shouldBe Walk(6_000)
        }
        "with extension method" in {
          import cats.Semigroup
          import cats.syntax.semigroup._
          case class Walk(stepsTaken: Int)
          implicit val semigroup: Semigroup[Walk] =
            (first: Walk, second: Walk) =>
              Walk(first.stepsTaken + second.stepsTaken)

          Walk(2_000) |+| Walk(4_000) shouldBe Walk(6_000)
        }
      }
    }
    "and indirectly; for example with" - {
      "lists of " - {

        def reduceWithImplicit[T](seq: Seq[T])(implicit
            semigroup: cats.Semigroup[T]
        ): T = seq.reduce(semigroup.combine)
        def reduceWithoutImplicit[T: cats.Semigroup](seq: Seq[T]): T = {
          import cats.syntax.semigroup._
          seq.reduce(_ |+| _)
        }

        "ints" - {
          "without extension method" in {
            import cats.Semigroup
            import cats.instances.int._
            val semigroup = Semigroup[Int]

            Seq(4, 6, 10, 12, 4, 6).reduce(semigroup.combine) shouldBe 42
          }
          "with extension method" in {
            import cats.instances.int._
            import cats.syntax.semigroup._

            Seq(4, 6, 10, 12, 4, 6).reduce(_ |+| _) shouldBe 42
          }
          "using a generic 'reduce' function (with implicit)" in {
            import cats.instances.int._

            reduceWithImplicit(Seq(4, 6, 10, 12, 4, 6)) shouldBe 42
          }
          "using a generic 'reduce' function (without implicit)" in {
            import cats.instances.int._

            reduceWithoutImplicit(Seq(4, 6, 10, 12, 4, 6)) shouldBe 42
          }
        }
        "strings" - {
          "without extension method" in {
            import cats.Semigroup
            import cats.instances.string._
            val semigroup = Semigroup[String]

            Seq("Hello", ",", " ", "World", "!").reduce(
              semigroup.combine
            ) shouldBe "Hello, World!"
          }
          "with extension method" in {
            import cats.instances.string._
            import cats.syntax.semigroup._

            Seq("Hello", ",", " ", "World", "!").reduce(
              _ |+| _
            ) shouldBe "Hello, World!"
          }
          "using a generic 'reduce' function (with implicit)" in {
            import cats.instances.string._

            reduceWithImplicit(
              Seq("Hello", ",", " ", "World", "!")
            ) shouldBe "Hello, World!"
          }
          "using a generic 'reduce' function (without implicit)" in {
            import cats.instances.string._

            reduceWithoutImplicit(
              Seq("Hello", ",", " ", "World", "!")
            ) shouldBe "Hello, World!"
          }
        }
        "custom type" - {
          case class Walk(stepsTaken: Int)
          "without extension method" in {
            import cats.Semigroup
            val semigroup = new Semigroup[Walk] {
              override def combine(first: Walk, second: Walk): Walk =
                Walk(first.stepsTaken + second.stepsTaken)
            }

            Seq(Walk(4_000), Walk(10_000), Walk(6_000)).reduce(
              semigroup.combine
            ) shouldBe Walk(20_000)
          }
          "with extension method" in {
            import cats.Semigroup
            import cats.syntax.semigroup._
            implicit val semigroup: Semigroup[Walk] =
              (first: Walk, second: Walk) =>
                Walk(first.stepsTaken + second.stepsTaken)

            Seq(Walk(4_000), Walk(10_000), Walk(6_000)).reduce(
              _ |+| _
            ) shouldBe Walk(20_000)
          }
          "using a generic 'reduce' function (with implicit)" in {
            import cats.Semigroup
            implicit val semigroup: Semigroup[Walk] =
              (first: Walk, second: Walk) =>
                Walk(first.stepsTaken + second.stepsTaken)
            reduceWithImplicit(
              Seq(Walk(4_000), Walk(10_000), Walk(6_000))
            ) shouldBe Walk(20_000)
          }
          "using a generic 'reduce' function (without implicit)" in {
            import cats.Semigroup
            implicit val semigroup: Semigroup[Walk] =
              (first: Walk, second: Walk) =>
                Walk(first.stepsTaken + second.stepsTaken)
            reduceWithoutImplicit(
              Seq(Walk(4_000), Walk(10_000), Walk(6_000))
            ) shouldBe Walk(20_000)
          }
        }
      }
    }
  }

}
