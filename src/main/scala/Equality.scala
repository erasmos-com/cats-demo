object Equality {

  /** Note that we cannot use the '===' from Cats directly in the tests, as the
    * one from Scalatic is brought in by Scalatest
    */
  object ExtensionMethodWrappers {

    def equalUsingExtensionMethod(first: Int, second: Int): Boolean = {
      import cats.instances.int._
      import cats.syntax.eq._
      first === second
    }

    def equalUsingExtensionMethod(
        first: List[Int],
        second: List[Int]
    ): Boolean = {
      import cats.instances.int._
      import cats.instances.list._
      import cats.syntax.eq._
      first === second
    }

    def equalUsingExtensionMethod(
        first: ColouredShape,
        second: ColouredShape
    ): Boolean = {
      import cats.syntax.eq._
      first === second
    }

  }

  case class ColouredShape(shape: String, colour: String)

  object ColouredShape {
    import cats.Eq
    implicit val colouredShapeEq: Eq[ColouredShape] =
      Eq.instance[ColouredShape] { (first, second) =>
        first.shape == second.shape
      }
  }
}
