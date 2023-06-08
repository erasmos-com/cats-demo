/** The type classes provided by Cats are invariant
  */
class InvariantTypeSpec extends DemoSpec {

  "when using these types classes, we see that they are invariant;" - {
    "for example, with" - {
      "(type safe) equality, between" - {
        "two Options" in {
          import cats.instances.int._
          import cats.instances.option._
          import cats.syntax.eq._
          val result = Option(12) eqv Option(10)
          result shouldBe false
        }
        "an Option and a None" in {
          import cats.instances.int._
          import cats.instances.option._
          import cats.syntax.eq._
          // Works, as None is case object of Option
          val result = Option(12) eqv None
          result shouldBe false
        }
        "two Somes" in {
          import cats.instances.int._
          import cats.instances.option._
          import cats.syntax.eq._
          // val result = Some(12) eqv Some(10) // Will not compile.
          // So we'll use the Option constructor instead.
          val result = Option(12) eqv Option(10)
          result shouldBe false
        }
        "a Some and a None" in {
          import cats.instances.int._
          import cats.instances.option._
          import cats.syntax.eq._
          // val result = Some(12) eqv None // Will not compile.
          // So we'll use the Option constructor instead.
          Option(12) shouldBe Some(12)
          val result = Option(12) eqv None
          result shouldBe false
        }
      }
    }
  }
}
