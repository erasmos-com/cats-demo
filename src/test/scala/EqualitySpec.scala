import EqualitySpec.ColouredShape
import EqualitySpec.ColouredShape.colouredShapeEq

/** The standard equality checks are not type-safe.
  *
  * Cats can provide this.
  */
class EqualitySpec extends DemoSpec {

  "when determining equality" - {

    "without cats," - {
      "the compiler will allow type-unsafe expressions; for example:" - {
        "this one, where we'll only get a warning" in {
          val result = 2 == "2" // Expect a compiler warning
          result shouldBe false
        }
      }
    }
    "with cats, however" - {
      "the compiler will catch it; for example" - {
        "between ints, whether using the" - {
          "type class, when" - {
            "equal" in {
              import cats.Eq
              import cats.instances.int._
              val intEquality: Eq[Int] = Eq[Int]
              val result               = intEquality.eqv(2, 3)
              // val anotherResult = intEquality.eqv(2, "two") // Won't compile
              result shouldBe false
            }
            "not equal" in {
              import cats.Eq
              import cats.instances.int._
              val intEquality: Eq[Int] = Eq[Int]
              val result               = intEquality.neqv(2, 3)
              // val anotherResult = intEquality.neqv(2, "two") // Won't compile
              result shouldBe true
            }
          }
          "extension method, when" - {
            "equal" in {
              import cats.instances.int._
              import cats.syntax.eq._
              val result = 2 eqv 3
              result shouldBe false
            }
            "not equal" in {
              import cats.instances.int._
              import cats.syntax.eq._
              val result = 2 =!= 3
              result shouldBe true
            }
          }
        }
        "between lists" - {
          "of ints, using the" - {
            "type class, when" - {
              "equal" in {
                import cats.Eq
                import cats.instances.int._
                import cats.instances.list._
                val listOfIntsEquality: Eq[List[Int]] = Eq[List[Int]]
                val result =
                  listOfIntsEquality.eqv(List(1, 2, 3), List(3, 4, 5))
                // val anotherResult = listOfIntsEquality.eqv(List(1, 2, 3), List("one", "two", "three")) // Won't compile
                result shouldBe false
              }
              "not equal" in {
                import cats.Eq
                import cats.instances.int._
                import cats.instances.list._
                val listOfIntsEquality: Eq[List[Int]] = Eq[List[Int]]
                val result =
                  listOfIntsEquality.neqv(List(1, 2, 3), List(3, 4, 5))
                // val anotherResult = listOfIntsEquality.neqv(List(1, 2, 3), List("one", "two", "three")) // Won't compile
                result shouldBe true
              }
            }
            "extension method, when" - {
              "equal" in {
                import cats.instances.int._
                import cats.instances.list._
                import cats.syntax.eq._
                val result = List(1, 2, 3) eqv List(3, 4, 5)
                result shouldBe false
              }
              "not equal" in {
                import cats.instances.int._
                import cats.instances.list._
                import cats.syntax.eq._
                val result = List(1, 2, 3) =!= List(3, 4, 5)
                result shouldBe true
              }
            }
          }
        }
        "between instances of a custom type, using the" - {
          val redSquare    = ColouredShape("square", "red")
          val yellowSquare = ColouredShape("square", "yellow")
          val yellowCircle = ColouredShape("circle", "yellow")
          "type class, when" - {
            "equal" in {
              val result = colouredShapeEq.eqv(redSquare, yellowSquare)
              result shouldBe true
            }
            "not equal" in {
              val result = colouredShapeEq.neqv(yellowSquare, yellowCircle)
              result shouldBe true
            }
          }
          "extension method, when" - {
            "equal" in {
              import cats.syntax.eq._
              val result = redSquare eqv yellowSquare
              result shouldBe true
            }
            "not equal" in {
              import cats.syntax.eq._
              val result = yellowSquare =!= yellowCircle
              result shouldBe true
            }
          }
        }
      }
    }
  }
}

object EqualitySpec {

  case class ColouredShape(shape: String, colour: String)

  object ColouredShape {

    import cats.Eq

    implicit val colouredShapeEq: Eq[ColouredShape] =
      Eq.instance[ColouredShape] { (first, second) =>
        first.shape == second.shape
      }
  }
}
