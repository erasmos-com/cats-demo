import StateSpec.ShoppingCart
import cats.Eval

/** A "state" is a wrapper over a single function.
  *
  * type MyState[S, A] = S => (S, A)
  *
  * S = The state. A = An answer, which is the result of a single computation.
  */
class StateSpec extends DemoSpec {

  "States" - {
    "wrap a single function which we can" - {
      import cats.data.State
      val countAndSay: State[Int, String] =
        State(currentCount => (currentCount + 1, s"Counted $currentCount"))
      "run" in {
        val result: Eval[(Int, String)] = countAndSay.run(4)

        result.value shouldBe (5, "Counted 4")
      }
      "inspect" in {
        val result: Eval[(Int, Int)] = countAndSay.inspect(_ * 2).run(4)

        result.value shouldBe (5, 10)
      }
      "get" in {
        val result: Eval[(Int, Int)] = countAndSay.get.run(4)

        result.value shouldBe (5, 5)
      }
      "modify" in {
        val result: Eval[(Int, String)] = countAndSay.modify(_ * 4).run(4)

        result.value shouldBe (20, "Counted 4")
      }
      "or all of the above" in {
        import cats.data.State._
        val program: State[Int, (Int, Int, Int)] = for {
          a <- get[Int]           // Initial (0)
          _ <- modify[Int](_ + 2) // State becomes 2
          b <- get[Int]           // 2, which is the final state
          c <- inspect[Int, Int](
            _ * 2
          ) // Returns 4, but doesn't update the state
        } yield (a, b, c)

        val result = program.run(0)

        result.value shouldBe (2, (0, 2, 4))
      }
    }
    "may be composed, by" - {
      "using simple plain functions" - {
        "using 'andThen'" in {
          val firstTransformation =
            (x: Int) => (x + 1, s"Added 1, obtaining ${x + 1}")
          val secondTransformation =
            (x: Int) => (x * 2, s"Multiplied by 2, obtaining ${x * 2}")
          val compositeTransformation =
            firstTransformation.andThen { case (newState, firstAnswer) =>
              val (finalState, secondAnswer) = secondTransformation(newState)
              (finalState, (firstAnswer, secondAnswer))
            }

          val result: (Int, (String, String)) = compositeTransformation(2)

          result shouldBe (6, (
            "Added 1, obtaining 3",
            "Multiplied by 2, obtaining 6"
          ))
        }
      }
      "using the 'State' type" - {
        "flatMap and map" in {
          import cats.data.State
          val firstTransformation: State[Int, String] =
            State((x: Int) => (x + 1, s"Added 1, obtaining ${x + 1}"))
          val secondTransformation: State[Int, String] =
            State((x: Int) => (x * 2, s"Multiplied by 2, obtaining ${x * 2}"))
          val compositeTransformation: State[Int, (String, String)] =
            firstTransformation.flatMap { (firstResult: String) =>
              secondTransformation.map((secondResult: String) =>
                (firstResult, secondResult)
              )
            }

          val result = compositeTransformation.run(2)

          result.value shouldBe (6, (
            "Added 1, obtaining 3",
            "Multiplied by 2, obtaining 6"
          ))
        }
        "a for comprehensions" in {
          import cats.data.State
          val firstTransformation: State[Int, String] =
            State((x: Int) => (x + 1, s"Added 1, obtaining ${x + 1}"))
          val secondTransformation: State[Int, String] =
            State((x: Int) => (x * 2, s"Multiplied by 2, obtaining ${x * 2}"))

          val compositeTransformation: State[Int, (String, String)] =
            for {
              firstResult  <- firstTransformation
              secondResult <- secondTransformation
            } yield (firstResult, secondResult)

          val result = compositeTransformation.run(2)

          result.value shouldBe (6, (
            "Added 1, obtaining 3",
            "Multiplied by 2, obtaining 6"
          ))
        }
      }
    }
    "used in larger structures, like" - {
      "a shopping cart, where we can" - {
        "add items" in {
          import cats.data.State
          val state: State[ShoppingCart, Double] =
            for {
              _          <- ShoppingCart.addToCart("Fender guitar", 500)
              _          <- ShoppingCart.addToCart("Elixir strings", 19)
              finalState <- ShoppingCart.addToCart("Electric cable", 8)
            } yield finalState

          val result: Eval[(ShoppingCart, Double)] =
            state.run(ShoppingCart(List.empty, 0))

          result.value shouldBe (ShoppingCart(
            List("Electric cable", "Elixir strings", "Fender guitar"),
            527
          ), 527)
        }
      }
    }
  }

}

object StateSpec {

  case class ShoppingCart(items: List[String], totalPrice: Double)

  object ShoppingCart {
    import cats.data.State
    def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
      State((shoppingCart: ShoppingCart) => {
        (
          ShoppingCart(
            item :: shoppingCart.items,
            shoppingCart.totalPrice + price
          ),
          price + shoppingCart.totalPrice
        )
      })

  }

}
