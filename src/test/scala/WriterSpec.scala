import WriterSpec.countWithLog

/** A Writer is useful for computing a value while logging at every step.
  */

class WriterSpec extends DemoSpec {

  "Using a Writer, like the one which logs" - {
    "as it counts" - {
      "can be run" - {
        "as-is" in {
          val result = countWithLog(4).run

          result shouldBe (Vector(
            "Starting",
            "At number [1]",
            "At number [2]",
            "At number [3]",
            "At number [4]"
          ), 4)
        }
        "transformed, with" - {
          "mapping the value" in {
            val result = countWithLog(4).map(_ + 2).run

            result shouldBe (Vector(
              "Starting",
              "At number [1]",
              "At number [2]",
              "At number [3]",
              "At number [4]"
            ), 6)
          }
          "mapping the logs" in {
            val result = countWithLog(4).mapWritten(_ :+ "Ending").run

            result shouldBe (Vector(
              "Starting",
              "At number [1]",
              "At number [2]",
              "At number [3]",
              "At number [4]",
              "Ending"
            ), 4)
          }
          "mapping both the value and the logs (using 'byMap')" in {
            val result = countWithLog(4).bimap(_ :+ "Ending", _ + 2).run

            result shouldBe (Vector(
              "Starting",
              "At number [1]",
              "At number [2]",
              "At number [3]",
              "At number [4]",
              "Ending"
            ), 6)
          }
          "mapping both the value and the logs (using 'mapBoth')" in {
            val result = countWithLog(4)
              .mapBoth((logs, value) => (logs :+ "Ending", value + 2))
              .run

            result shouldBe (Vector(
              "Starting",
              "At number [1]",
              "At number [2]",
              "At number [3]",
              "At number [4]",
              "Ending"
            ), 6)
          }
        }
      }
      "can be composed" in {
        import cats.instances.vector._ // Brings in the Semigroup for Vector
        val firstWriter  = countWithLog(2)
        val secondWriter = countWithLog(4)
        val compositeWriter =
          for {
            firstValue  <- firstWriter
            secondValue <- secondWriter
          } yield firstValue + secondValue

        val result = compositeWriter.run

        result shouldBe (Vector(
          "Starting",
          "At number [1]",
          "At number [2]",
          "Starting",
          "At number [1]",
          "At number [2]",
          "At number [3]",
          "At number [4]"
        ), 6)
      }
      "can be reset" in {
        import cats.instances.vector._ // Required for the Monoid for Vector
        val result = countWithLog(4).reset.run

        result shouldBe (Vector.empty[String], 4)
      }
      "can be decomposed into" - {
        "the logs" in {
          val result = countWithLog(4)

          result.written shouldBe Vector(
            "Starting",
            "At number [1]",
            "At number [2]",
            "At number [3]",
            "At number [4]"
          )
        }
        "the value" in {
          val result = countWithLog(4)

          result.value shouldBe 4
        }
      }
    }
  }

}

object WriterSpec {

  import cats.data.Writer
  import cats.instances.vector._

  def countWithLog(n: Int): Writer[Vector[String], Int] =
    if (n <= 0) Writer(Vector("Starting"), 0)
    else
      countWithLog(n - 1).flatMap(numberWeIgnore =>
        Writer(Vector(s"At number [$n]"), n)
      )

}
