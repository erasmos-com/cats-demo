import scala.util.{Success, Try}

/** A functor provides a map method, suitable for applying a sequence of
  * transformations.
  */
class FunctorSpec extends DemoSpec {

  "functors apply to different types, such as" - {
    "a list, using" - {
      "a specific functor" in {
        import cats.Functor
        import cats.instances.list._
        val functor: Functor[List] = Functor[List]
        val original               = List(10, 20, 30, 40)

        val result = functor.map(original)(_ + 2)

        result shouldBe List(12, 22, 32, 42)
      }
      "a general function (version one)" in {
        import cats.instances.list._
        val original = List(10, 20, 30, 40)

        val result = FunctorSpec.addTwoVersionOne(original)

        result shouldBe List(12, 22, 32, 42)
      }
      "a general function (version two)" in {
        import cats.instances.list._
        val original = List(10, 20, 30, 40)

        val result = FunctorSpec.addTwoVersionTwo(original)

        result shouldBe List(12, 22, 32, 42)
      }
    }
    "an option" - {
      "a specific functor" in {
        import cats.Functor
        import cats.instances.option._
        val functor: Functor[Option] = Functor[Option]
        val original                 = Option(40)

        val result = functor.map(original)(_ + 2)

        result shouldBe Some(42)
      }
      "a general function (version one)" in {
        import cats.instances.option._
        val original = Option(40)

        val result = FunctorSpec.addTwoVersionOne(original)

        result shouldBe Some(42)
      }
      "a general function (version two)" in {
        import cats.instances.option._
        val original = Option(40)

        val result = FunctorSpec.addTwoVersionTwo(original)

        result shouldBe Some(42)
      }
    }
    "a try" - {
      "a specific functor" in {
        import cats.Functor
        import cats.instances.try_._
        val functor: Functor[Try] = Functor[Try]
        val original              = Try(40)

        val result = functor.map(original)(_ + 2)

        result shouldBe Success(42)
      }
      "a general function (version one)" in {
        import cats.instances.try_._
        val original = Try(40)

        val result = FunctorSpec.addTwoVersionOne(original)

        result shouldBe Success(42)
      }
      "a general function (version two)" in {
        import cats.instances.try_._
        val original = Try(40)

        val result = FunctorSpec.addTwoVersionTwo(original)

        result shouldBe Success(42)
      }
    }
    "a custom type" - {
      "a specific functor" in {
        import FunctorSpec._
        import cats.Functor
        val functor: Functor[Tree] = TreeFunctor
        val original =
          Tree.branch(
            30,
            Tree.leaf(10),
            FunctorSpec.Tree
              .branch(20, FunctorSpec.Tree.leaf(2), FunctorSpec.Tree.leaf(4))
          )

        val result = functor.map(original)(_ + 2)

        result shouldBe
          Tree.branch(
            32,
            Tree.leaf(12),
            FunctorSpec.Tree
              .branch(22, FunctorSpec.Tree.leaf(4), FunctorSpec.Tree.leaf(6))
          )
      }
      "a specific functor (using an extension method)" in {
        import FunctorSpec._
        import cats.syntax.functor._
        val original =
          Tree.branch(
            30,
            Tree.leaf(10),
            FunctorSpec.Tree
              .branch(20, FunctorSpec.Tree.leaf(2), FunctorSpec.Tree.leaf(4))
          )

        val result = original.map(_ + 2)

        result shouldBe Tree.branch(
          32,
          Tree.leaf(12),
          FunctorSpec.Tree
            .branch(22, FunctorSpec.Tree.leaf(4), FunctorSpec.Tree.leaf(6))
        )
      }
      "a general function (version one)" in {
        import FunctorSpec._
        val original = Tree.branch(
          30,
          Tree.leaf(10),
          FunctorSpec.Tree
            .branch(20, FunctorSpec.Tree.leaf(2), FunctorSpec.Tree.leaf(4))
        )

        val result = FunctorSpec.addTwoVersionOne(original)

        result shouldBe
          Tree.branch(
            32,
            Tree.leaf(12),
            FunctorSpec.Tree
              .branch(22, FunctorSpec.Tree.leaf(4), FunctorSpec.Tree.leaf(6))
          )
      }
      "a general function (version two)" in {
        import FunctorSpec._
        val original =
          Tree.branch(
            30,
            Tree.leaf(10),
            FunctorSpec.Tree
              .branch(20, FunctorSpec.Tree.leaf(2), FunctorSpec.Tree.leaf(4))
          )

        val result = FunctorSpec.addTwoVersionTwo(original)

        result shouldBe
          Tree.branch(
            32,
            Tree.leaf(12),
            FunctorSpec.Tree
              .branch(22, FunctorSpec.Tree.leaf(4), FunctorSpec.Tree.leaf(6))
          )
      }
    }
  }

}

object FunctorSpec {

  import cats.Functor

  def addTwoVersionOne[F[_]](container: F[Int])(implicit
      functor: Functor[F]
  ): F[Int] =
    functor.map(container)(_ + 2)

  def addTwoVersionTwo[F[_]: Functor](container: F[Int]): F[Int] = {
    import cats.syntax.functor._
    container.map(_ + 2)
  }

  trait Tree[+T]

  case class Leaf[+T](value: T) extends Tree[T]

  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  object Tree {
    def leaf[T](value: T): Leaf[T] = Leaf(value)

    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] =
      Branch(value, left, right)
  }

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(value, leftTree, rightTree) =>
        Branch(f(value), map(leftTree)(f), map(rightTree)(f))
    }
  }

}
