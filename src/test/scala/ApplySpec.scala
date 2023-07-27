import cats.{Applicative, Apply}

/** Extended by Applicative.
  *
  * Rarely used by itself, but useful for extracting and combining tuples.
  */
class ApplySpec extends DemoSpec {

  "Apply" - {
    "extends" - {
      "Functor" in {
        import cats.instances.option._
        val apply = Apply[Option]

        apply.isInstanceOf[cats.Functor[Option]] shouldBe true
      }
      "Semigroupal" in {
        import cats.instances.option._
        val apply = Apply[Option]

        apply.isInstanceOf[cats.Semigroupal[Option]] shouldBe true
      }
    }
    "is extended by" - {
      "Applicative" in {
        import cats.instances.option._
        val applicative = Applicative[Option]

        applicative.isInstanceOf[Apply[Option]] shouldBe true
      }
    }
    "allows automatic unwrapping and" - {
      "transformation; for example:" - {
        "applying an arbitrary function" - {
          "to one wrapped element, namely" - {
            "an Option wrapping an int" in {
              import cats.Apply
              import cats.instances.option._
              val apply = Apply[Option]

              val result = apply.ap(Some((x: Int) => x + 1))(Some(2))

              result shouldBe Some(3)
            }
            "an Option wrapping a string" in {
              import cats.Apply
              import cats.instances.option._
              val apply = Apply[Option]

              val result =
                apply.ap(Some((x: String) => x.reverse))(Some("Hello"))

              result shouldBe Some("olleH")
            }
          }
          "to numerous wrapped elements, namely" - {
            "an Option wrapping an int" - {
              "explicitly" - {
                import cats.Apply
                import cats.instances.option._
                val apply = Apply[Option]

                val result =
                  apply.map3(Option(1), Option(2), Option(4))(_ + _ + _)

                result shouldBe Some(7)
              }
              "using an extension method" in {
                import cats.instances.option._
                import cats.syntax.apply._

                val result = (Option(1), Option(2), Option(4)).mapN(_ + _ + _)

                result shouldBe Some(7)
              }
            }
            "an Option wrapping a string" - {
              "explicitly" in {
                import cats.Apply
                import cats.instances.option._
                val apply = Apply[Option]

                val result = apply.map3(Option("A"), Option("B"), Option("C"))(
                  _ + "-" + _ + "-" + _
                )

                result shouldBe Some("A-B-C")
              }
              "using an extension method" in {
                import cats.instances.option._
                import cats.syntax.apply._

                val result = (Option("A"), Option("B"), Option("C")).mapN(
                  _ + "-" + _ + "-" + _
                )

                result shouldBe Some("A-B-C")
              }
            }
          }
        }
      }
      "inversion of tuples with" - {
        "an Option wrapping an int" - {
          "explicitly" in {
            import cats.Apply
            import cats.instances.option._
            val apply = Apply[Option]

            val result: Option[(Int, Int, Int)] =
              apply.tuple3(Option(1), Option(2), Option(4))

            result shouldBe Option((1, 2, 4))
          }
          "using extension method" in {
            import cats.instances.option._
            import cats.syntax.apply._
            val result: Option[(Int, Int, Int)] =
              (Option(1), Option(2), Option(4)).tupled

            result shouldBe Option((1, 2, 4))
          }
        }
        "an Option wrapping a string" - {
          "explicitly" in {
            import cats.Apply
            import cats.instances.option._
            val apply = Apply[Option]

            val result = apply.tuple3(Option("A"), Option("B"), Option("C"))

            result shouldBe Option(("A", "B", "C"))
          }
          "using extension method" in {
            import cats.instances.option._
            import cats.syntax.apply._

            val result = (Option("A"), Option("B"), Option("C")).tupled

            result shouldBe Option(("A", "B", "C"))
          }
        }
      }
      "creation of tuples (via 'product') with" - {
        "an Option wrapping an int" - {
          import cats.Apply
          import cats.instances.option._
          val apply = Apply[Option]

          val result = apply.product(Some(4), Some(10))

          result shouldBe Some((4, 10))
        }
        "an Option wrapping a string" - {
          import cats.Apply
          import cats.instances.option._
          val apply = Apply[Option]

          val result = apply.product(Some("Hello"), Some("Goodbye"))

          result shouldBe Some(("Hello", "Goodbye"))
        }
      }
    }
  }

}
