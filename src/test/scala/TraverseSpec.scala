import TraverseSpec.{ApplicativeBased, MonadBased}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

/** A higher-kinded type which offers functions which can turning data
  * structures inside-out, even when nested.
  *
  * Also useful for general data combinations.
  *
  * The minimum structure required for traversing in an Applicative; we don't
  * require a Monad.
  */

class TraverseSpec extends DemoSpec {

  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  "traversing offers" - {
    "the 'inversion' of various data structures" - {
      "using" - {

        def calculateHealthIndex(fruit: String): Future[Int] =
          Future(fruit.length)
        val fruits = List("Apple", "Orange", "Banana", "Grape")

        "'sequence' + map" in {
          val listOfFutures: List[Future[Int]] =
            fruits.map(calculateHealthIndex)

          val result: Future[List[Int]] = Future.sequence(listOfFutures)

          await(result) shouldBe List(5, 6, 6, 5)
        }
        "simply 'traverse'" in {
          val result: Future[List[Int]] =
            Future.traverse(fruits)(calculateHealthIndex)

          await(result) shouldBe List(5, 6, 6, 5)
        }
        "a direct foldLeft" in {
          val result = fruits.foldLeft(Future(List.empty[Int])) {
            (healthIndicesFuture: Future[List[Int]], fruit: String) =>
              for {
                healthIndices: List[Int] <- healthIndicesFuture
                healthIndex: Int         <- calculateHealthIndex(fruit)
              } yield healthIndices :+ healthIndex
          }

          await(result) shouldBe List(5, 6, 6, 5)
        }
        "a foldLeft wrapped in a Monad" in {
          import cats.instances.future._ // For Monad[Future]

          val result =
            MonadBased.traverse[Future, String, Int](fruits)(
              calculateHealthIndex
            )

          await(result) shouldBe List(5, 6, 6, 5)
        }
        "a foldLeft wrapped in an Applicative (traverse)" in {
          import cats.instances.future._ // For Applicative[Future]

          val result: Future[List[Int]] =
            ApplicativeBased.traverse[Future, String, Int](fruits)(
              calculateHealthIndex
            )

          await(result) shouldBe List(5, 6, 6, 5)
        }
        "a foldLeft wrapped in an Applicative (sequence)" in {
          import cats.instances.vector._ // For Applicative[Vector]
          val list = List(Vector(1, 2), Vector(3, 4))

          val result: Vector[List[Int]] = ApplicativeBased.sequence(list)

          result shouldBe Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
        }
        "Traverse, itself" - {
          "directly" in {
            import cats.Traverse
            import cats.instances.future._
            import cats.instances.list._ // For Applicative[Future]
            val traverse = Traverse[List]

            val result: Future[List[Int]] =
              traverse.traverse(fruits)(calculateHealthIndex)

            await(result) shouldBe List(5, 6, 6, 5)
          }
          "with extension method" in {
            import cats.instances.future._
            import cats.instances.list._
            import cats.syntax.traverse._

            val result: Future[List[Int]] =
              fruits.traverse(calculateHealthIndex)

            await(result) shouldBe List(5, 6, 6, 5)
          }
        }
      }
    }
    "filtering when ensuring all match a predicate, with" - {

      val isEven = (n: Int) => n % 2 == 0

      "Option when" - {
        import cats.Traverse
        import cats.instances.list._
        import cats.instances.option._ // For ?

        def filter(list: List[Int])(
            predicate: Int => Boolean
        ): Option[List[Int]] =
          Traverse[List].traverse(list)(n => Some(n).filter(predicate))

        "all are true" in {
          val list = List(2, 4, 6)

          val result = filter(list)(isEven)

          result shouldBe Some(List(2, 4, 6))
        }
        "only some are true" in {
          val list = List(2, 3, 4, 6)

          val result = filter(list)(isEven)

          result shouldBe None
        }
      }
      "Validated when" - {
        import cats.Traverse
        import cats.data.Validated
        import cats.instances.list._ // For Traverse[List]
        type ErrorsOr[T] = Validated[List[String], T]

        def filter(
            list: List[Int]
        )(predicate: Int => Boolean): ErrorsOr[List[Int]] =
          Traverse[List].traverse(list) { n =>
            if (predicate(n)) Validated.valid(n)
            else Validated.invalid(List(s"Predicate failed for [$n]"))
          }

        "all are true" in {
          val list = List(2, 4, 6)

          val result = filter(list)(isEven)

          result shouldBe Validated.Valid(List(2, 4, 6))
        }
        "only some are true" in {
          val list = List(2, 3, 4, 6, 7)

          val result = filter(list)(isEven)

          result shouldBe Validated.Invalid(
            List("Predicate failed for [3]", "Predicate failed for [7]")
          )
        }
      }
    }
  }
}

object TraverseSpec {

  object ApplicativeBased {
    import cats.Applicative
    import cats.syntax.applicative._
    import cats.syntax.apply._ // For mapN
    def traverse[F[_]: Applicative, A, B](
        list: List[A]
    )(func: A => F[B]): F[List[B]] =
      list.foldLeft(List.empty[B].pure[F]) {
        (wrappedAccumulator, wrappedElement) =>
          (wrappedAccumulator, func(wrappedElement)).mapN(_ :+ _)
      }

    def sequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
      traverse(list)(identity)
  }

  object MonadBased {
    import cats.Monad
    import cats.syntax.applicative._
    import cats.syntax.flatMap._
    import cats.syntax.functor._ // For Monad[Future]

    def traverse[F[_]: Monad, A, B](
        list: List[A]
    )(func: A => F[B]): F[List[B]] =
      list.foldLeft(List.empty[B].pure[F]) {
        (wrappedAccumulator, wrappedElement) =>
          {
            for {
              unwrappedAccumulator        <- wrappedAccumulator
              transformedUnwrappedElement <- func(wrappedElement)
            } yield unwrappedAccumulator :+ transformedUnwrappedElement
          }
      }

  }
}
