import EvalSpec.reverseList
import cats.Eval

class EvalSpec extends DemoSpec {

  "Evaluation" - {
    "has three modes, which are" - {
      "eager" in {
        var numberOfCalls = 0
        val eval = Eval.now {
          numberOfCalls += 1
          88
        }

        numberOfCalls shouldBe 1
        eval.value shouldBe 88
        numberOfCalls shouldBe 1
      }
      "lazy" - {
        "with memoization, using" - {
          "later" in {
            var numberOfCalls = 0
            val eval = Eval.later {
              numberOfCalls += 1
              88
            }

            numberOfCalls shouldBe 0
            eval.value shouldBe 88
            numberOfCalls shouldBe 1
            eval.value shouldBe 88
            numberOfCalls shouldBe 1
          }
          "always + memoize" in {
            var numberOfCalls = 0
            val eval = Eval.always {
              numberOfCalls += 1
              88
            }.memoize

            numberOfCalls shouldBe 0
            eval.value shouldBe 88
            numberOfCalls shouldBe 1
            eval.value shouldBe 88
            numberOfCalls shouldBe 1
          }
        }
        "without memoization, using" - {
          "always" in {
            var numberOfCalls = 0
            val eval = Eval.always {
              numberOfCalls += 1
              88
            }

            numberOfCalls shouldBe 0
            eval.value shouldBe 88
            numberOfCalls shouldBe 1
            eval.value shouldBe 88
            numberOfCalls shouldBe 2
          }
          "a defer wrapper" in {
            var numberOfCalls = 0
            val eval = Eval.defer(Eval.now {
              numberOfCalls += 1
              88
            })

            numberOfCalls shouldBe 0
            eval.value shouldBe 88
            numberOfCalls shouldBe 1
            eval.value shouldBe 88
            numberOfCalls shouldBe 2
          }
        }
      }
    }
    "can be composed" in {
      var numberOfCalls = 0
      val instantEval: Eval[Int] = Eval.now {
        numberOfCalls += 1
        10
      }

      numberOfCalls shouldBe 1

      val lazyWithMemoizationEval: Eval[Int] = Eval.later {
        numberOfCalls += 1
        20
      }

      numberOfCalls shouldBe 1

      val composedEval: Eval[Int] =
        for {
          valueOne <- instantEval
          valueTwo <- lazyWithMemoizationEval
        } yield valueOne + valueTwo

      numberOfCalls shouldBe 1
      composedEval.value shouldBe 30
      numberOfCalls shouldBe 2
      composedEval.value shouldBe 30
      numberOfCalls shouldBe 2

    }
    "can help to avoid stack overflow" in {
      val numbers = (1 to 10_000).toList

      val result: Eval[List[Int]] = reverseList(numbers)

      result.value.length shouldBe 10_000
      result.value.head shouldBe 10_000
    }
  }

}

object EvalSpec {

  def reverseList[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else Eval.defer(reverseList(list.tail).map(_ :+ list.head))

}
