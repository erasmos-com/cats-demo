class FlatMapSpec extends DemoSpec {

  "FlatMap" - {
    "extends" - {
      "Apply" in {
        import cats.instances.option._
        import cats.{Apply, FlatMap}
        val flatMap = FlatMap[Option]

        flatMap.isInstanceOf[Apply[Option]] shouldBe true
      }
    }
    "maps and flattens, using various wrapper types, such as" - {
      "an Option" - {
        "explicitly" in {
          import cats.FlatMap
          import cats.instances.option._
          val flatMap = FlatMap[Option]

          val result = flatMap.flatMap(Some(4))(x => Some(x * 2))

          result shouldBe Some(8)
        }
        "in a for comprehension" in {
          val result = for {
            first  <- Some(4)
            second <- Some(2)
          } yield first * second

          result shouldBe Some(8)
        }
      }
      "a Try" - {
        "explicitly" in {
          import cats.FlatMap
          import cats.instances.try_._

          import scala.util.{Success, Try}
          val flatMap = FlatMap[Try]

          val result = flatMap.flatMap(Success(4))(x => Success(x * 2))

          result shouldBe Success(8)
        }
        "in a for comprehension" in {
          import scala.util.Success
          val result = for {
            first  <- Success(4)
            second <- Success(2)
          } yield first * second

          result shouldBe Success(8)
        }
      }
    }
  }

}
