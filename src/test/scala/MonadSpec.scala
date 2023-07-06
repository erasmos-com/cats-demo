import MonadSpec.HttpService.{HttpServiceUsingOption, HttpServiceUsingTry}
import MonadSpec.TrafficSurgeDetector.AsyncResponse
import MonadSpec._
import cats.Monad

import java.util.concurrent.Executors
import scala.annotation.unused
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** A monad provides both a pure method - which wraps a normal value into a
  * monadic one - as well as a flat map, which transforms monadic values (aka a
  * chained transformation); often this is done on a sequence, but not always.
  * From these two functions, one can derive 'map'.
  *
  * Some use cases: list combinations, option transformers, asynchronous chained
  * computations and dependent computations.
  */
class MonadSpec extends DemoSpec {

  private implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  "Monads can be used" - {
    "for transforming values, acting upon" - {
      "a list using" - {
        val numbers = List(1, 2)
        "map (built-in)" in {
          numbers.map(_ + 1) shouldBe List(2, 3)
        }
        "map (from Functor, a parent of Monad)" in {
          import cats.Functor
          import cats.instances.list._
          val functor = Functor[List]

          functor.map(numbers)(_ + 1) shouldBe List(2, 3)
        }
        "map (from Monad)" in {
          import cats.Monad
          import cats.instances.list._
          val monad = Monad[List]

          monad.map(numbers)(_ + 1) shouldBe List(2, 3)
        }
        "flatMap and pure (from Monad)" in {
          import cats.Monad
          import cats.instances.list._
          val monad = Monad[List]

          monad.flatMap(numbers)(x => monad.pure(x + 1)) shouldBe List(2, 3)
        }
      }
      "an option using" - {
        val numberOpt = Option(1)
        "map (built-in)" in {
          numberOpt.map(_ + 1) shouldBe Option(2)
        }
        "map (from Functor, a parent of Monad)" in {
          import cats.Functor
          import cats.instances.option._
          val functor = Functor[Option]

          functor.map(numberOpt)(_ + 1) shouldBe Option(2)
        }
        "map (from Monad)" in {
          import cats.Monad
          import cats.instances.option._
          val monad = Monad[Option]

          monad.map(numberOpt)(_ + 1) shouldBe Option(2)
        }
        "flatMap and pure (from Monad)" in {
          import cats.Monad
          import cats.instances.option._
          val monad = Monad[Option]

          monad.flatMap(numberOpt)(x => monad.pure(x + 1)) shouldBe Option(2)
        }
      }
      "a future using" - {
        val numberFut = Future(1)
        "map (built-in)" in {
          await(numberFut.map(_ + 1)) shouldBe 2
        }
        "map (from Functor, a parent of Monad)" in {
          import cats.Functor
          import cats.instances.future._
          val functor = Functor[Future]

          await(functor.map(numberFut)(_ + 1)) shouldBe 2
        }
        "map (from Monad)" in {
          import cats.Monad
          import cats.instances.future._
          val monad = Monad[Future]

          await(monad.map(numberFut)(_ + 1)) shouldBe 2
        }
        "flatMap and pure (from Monad)" in {
          import cats.Monad
          import cats.instances.future._
          val monad = Monad[Future]

          await(monad.flatMap(numberFut)(x => monad.pure(x + 1))) shouldBe 2
        }
      }
      "an either using" - {
        type SuccessOrError[T] = Either[String, T]
        val right: SuccessOrError[Int] = Right(3)
        val left: SuccessOrError[Int]  = Left("Error")
        "map (built-in) on" - {
          "right" in {
            right.map(_ + 1) shouldBe Right(4)
          }
          "left" in {
            left.map(_ + 1) shouldBe Left("Error")
          }
        }
        "map (from Functor, a parent of Monad) on" - {
          "right" in {
            import cats.Functor
            import cats.instances.either._
            val functor = Functor[SuccessOrError]

            functor.map(right)(_ + 1) shouldBe Right(4)
          }
          "left" in {
            import cats.Functor
            import cats.instances.either._
            val functor = Functor[SuccessOrError]

            functor.map(left)(_ + 1) shouldBe Left("Error")
          }
        }
        "map (from Monad) on" - {
          "right" in {
            import cats.Monad
            import cats.instances.either._
            val monad = Monad[SuccessOrError]

            monad.map(right)(_ + 1) shouldBe Right(4)
          }
          "left" in {
            import cats.Monad
            import cats.instances.either._
            val monad = Monad[SuccessOrError]

            monad.map(left)(_ + 1) shouldBe Left("Error")
          }
        }
        "flatMap and pure (from Monad) on" - {
          "right" in {
            import cats.Monad
            import cats.instances.either._
            val monad = Monad[SuccessOrError]

            monad.flatMap(right)(x => monad.pure(x + 1)) shouldBe Right(4)
          }
          "left" in {
            import cats.Monad
            import cats.instances.either._
            val monad = Monad[SuccessOrError]

            monad.flatMap(left)(x => monad.pure(x + 1)) shouldBe Left("Error")
          }
        }
      }
      "a custom type using" - {
        "flatMap" in {
          val tree: Tree[Int] = Branch(Leaf(10), Branch(Leaf(2), Leaf(4)))

          val result =
            TreeMonad.flatMap(tree)(x => Branch(Leaf(x + 1), Leaf(x + 2)))

          result shouldBe Branch(
            Branch(Leaf(11), Leaf(12)),
            Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Leaf(6)))
          )
        }
        "map, directly" in {
          val tree: Tree[Int] = Branch(Leaf(10), Leaf(20))

          val result = TreeMonad.map(tree)(x => x * 2)

          result shouldBe Branch(Leaf(20), Leaf(40))
        }
        "map, using flatMap and pure" in {
          val tree: Tree[Int] = Branch(Leaf(10), Leaf(20))

          val result = TreeMonad.flatMap(tree)(x => TreeMonad.pure(x * 2))

          result shouldBe Branch(Leaf(20), Leaf(40))
        }

      }
    }
    "for generating combinations, acting upon" - {
      "a list using" - {
        val numbers = List(1, 2)
        val chars   = List('a', 'b')
        "flatMap (built-in)" in {
          val combinations: List[(Int, Char)] =
            numbers.flatMap(n => chars.map(c => (n, c)))

          combinations shouldBe List(
            (1, 'a'),
            (1, 'b'),
            (2, 'a'),
            (2, 'b')
          )
        }
        "flatMap (from Monad)" in {
          import cats.Monad
          import cats.instances.list._
          val monad = Monad[List]

          val combinations =
            monad.flatMap(numbers)(n => monad.map(chars)(c => (n, c)))

          combinations shouldBe List(
            (1, 'a'),
            (1, 'b'),
            (2, 'a'),
            (2, 'b')
          )
        }
        "a for comprehension" in {
          val combinations: List[(Int, Char)] =
            for {
              n <- numbers
              c <- chars
            } yield (n, c)

          combinations shouldBe List(
            (1, 'a'),
            (1, 'b'),
            (2, 'a'),
            (2, 'b')
          )
        }
        "our typed function" in {
          val combinations = MonadSpec.asPairsList(numbers, chars)

          combinations shouldBe List(
            (1, 'a'),
            (1, 'b'),
            (2, 'a'),
            (2, 'b')
          )

        }
        "our generic function (version one)" in {
          import cats.Monad
          import cats.instances.list._
          @unused
          implicit val monad: Monad[List] = Monad[List]
          val combinations = MonadSpec.asPairsGenericOne(numbers, chars)

          combinations shouldBe List(
            (1, 'a'),
            (1, 'b'),
            (2, 'a'),
            (2, 'b')
          )
        }
        "our generic function (version two)" in {
          import cats.Monad
          import cats.instances.list._
          @unused
          implicit val monad: Monad[List] = Monad[List]
          val combinations = MonadSpec.asPairsGenericTwo(numbers, chars)

          combinations shouldBe List(
            (1, 'a'),
            (1, 'b'),
            (2, 'a'),
            (2, 'b')
          )
        }
      }
      "an option using" - {
        val numberOpt: Option[Int] = Option(2)
        val charOpt: Option[Char]  = Option('d')
        "flatMap (built-in)" in {
          val combinations: Option[(Int, Char)] =
            numberOpt.flatMap(n => charOpt.map(c => (n, c)))

          combinations shouldBe Some((2, 'd'))
        }
        "flatMap (from Monad)" in {
          import cats.Monad
          import cats.instances.option._
          val monad = Monad[Option]

          val combinations =
            monad.flatMap(numberOpt)(n => monad.map(charOpt)(c => (n, c)))

          combinations shouldBe Some((2, 'd'))
        }
        "a for comprehension" in {
          val combinations: Option[(Int, Char)] =
            for {
              n <- numberOpt
              c <- charOpt
            } yield (n, c)

          combinations shouldBe Some((2, 'd'))
        }
        "our typed function" in {
          val combinations = MonadSpec.asPairsOption(numberOpt, charOpt)

          combinations shouldBe Some((2, 'd'))
        }
        "our generic function (version one)" in {
          import cats.Monad
          import cats.instances.option._
          @unused
          implicit val monad: Monad[Option] = Monad[Option]
          val combinations = MonadSpec.asPairsGenericOne(numberOpt, charOpt)

          combinations shouldBe Some((2, 'd'))
        }
        "our generic function (version two)" in {
          import cats.Monad
          import cats.instances.option._
          @unused
          implicit val monad: Monad[Option] = Monad[Option]
          val combinations = MonadSpec.asPairsGenericTwo(numberOpt, charOpt)

          combinations shouldBe Some((2, 'd'))
        }
      }
      "a Future using" - {
        val numberFut: Future[Int] = Future(2)
        val charFut: Future[Char]  = Future('d')
        "flatMap (built-in)" in {
          val combinationsFut: Future[(Int, Char)] =
            numberFut.flatMap(n => charFut.map(c => (n, c)))

          await(combinationsFut) shouldBe (2, 'd')
        }
        "flatMap (from Monad)" in {
          import cats.Monad
          import cats.instances.future._
          val monad = Monad[Future]

          val combinationsFut =
            monad.flatMap(numberFut)(n => monad.map(charFut)(c => (n, c)))

          await(combinationsFut) shouldBe (2, 'd')
        }
        "a for comprehension" in {
          val combinationsFut: Future[(Int, Char)] =
            for {
              n <- numberFut
              c <- charFut
            } yield (n, c)

          await(combinationsFut) shouldBe (2, 'd')
        }
        "our typed function" in {
          val combinationsFut = asPairsFuture(numberFut, charFut)

          await(combinationsFut) shouldBe (2, 'd')
        }
        "our generic function (version one)" in {
          import cats.Monad
          import cats.instances.future._
          @unused
          implicit val monad: Monad[Future] = Monad[Future]
          val combinationsFut = asPairsGenericOne(numberFut, charFut)

          await(combinationsFut) shouldBe (2, 'd')
        }
        "our generic function (version two)" in {
          import cats.Monad
          import cats.instances.future._
          @unused
          implicit val monad: Monad[Future] = Monad[Future]
          val combinationsFut = asPairsGenericTwo(numberFut, charFut)

          await(combinationsFut) shouldBe (2, 'd')
        }
      }
      "a list of options" in {
        import cats.data.OptionT
        import cats.instances.list._
        val numberOptions: OptionT[List, Int] =
          OptionT(List(Option(1), Option(2)))
        val charOptions: OptionT[List, Char] =
          OptionT(List(Option('a'), Option('b'), Option.empty[Char]))

        val combinations = for {
          number <- numberOptions
          char   <- charOptions
        } yield (number, char)

        combinations.value shouldBe List(
          Some((1, 'a')),
          Some((1, 'b')),
          None,
          Some((2, 'a')),
          Some((2, 'b')),
          None
        )
      }
    }
    "for wrapping simple values with 'pure', acting upon" - {
      "a list, using" - {
        "an explicit Monad" in {
          import cats.Monad
          import cats.instances.list._
          val monad = Monad[List]

          monad.pure(3) shouldBe List(3)
        }
        "an extension method" in {
          import cats.instances.list._
          import cats.syntax.applicative._

          3.pure[List] shouldBe List(3)
        }
      }
      "an option" - {
        "an explicit Monad" in {
          import cats.Monad
          import cats.instances.option._
          val monad = Monad[Option]

          monad.pure(3) shouldBe Some(3)
        }
        "an extension method" in {
          import cats.instances.option._
          import cats.syntax.applicative._

          3.pure[Option] shouldBe Some(3)
        }
      }
      "a future" - {

        "an explicit Monad" in {
          import cats.Monad
          import cats.instances.future._
          val monad = Monad[Future]

          await(monad.pure(3)) shouldBe 3
        }
        "an extension method" in {
          import cats.instances.future._
          import cats.syntax.applicative._

          await(3.pure[Future]) shouldBe 3
        }
      }
      "a custom type" - {
        "an explicit Monad" in {
          TreeMonad.pure(4) shouldBe Leaf(4)
        }
        "an extension method" in {
          import cats.syntax.applicative._
          implicit val monad = TreeMonad
          4.pure shouldBe Leaf(4)
        }
      }
    }
    "to build larger types, for example" - {
      "an Http Server, using" - {
        "an option, when" - {
          val httpService = HttpServiceUsingOption
          "a connection" - {
            "can be created" in {
              val connectionOpt = httpService.getConnection(
                Map("host" -> "www.somewhere.com", "port" -> "8080")
              )

              connectionOpt shouldBe Some(
                MonadSpec.Connection("www.somewhere.com", "8080")
              )
            }
            "cannot be created" in {
              val connectionOpt =
                httpService.getConnection(Map("host" -> "www.somewhere.com"))

              connectionOpt shouldBe None
            }
          }
          "a request payload is" - {
            val connection = MonadSpec.Connection("www.somewhere.com", "8080")
            "valid" in {
              val responseOpt = httpService.issueRequest(connection, "Valid")

              responseOpt shouldBe Some("Payload accepted: [Valid]")
            }
            "invalid" in {
              val responseOpt = httpService.issueRequest(
                connection,
                "Invalid (as it's much too long)"
              )

              responseOpt shouldBe None
            }
          }
        }
        "a try, when" - {
          val httpService = HttpServiceUsingTry
          "a connection" - {
            "can be created" in {
              val connectionTry = httpService.getConnection(
                Map("host" -> "www.somewhere.com", "port" -> "8080")
              )

              connectionTry shouldBe Success(
                MonadSpec.Connection("www.somewhere.com", "8080")
              )
            }
            "cannot be created" in {
              val connectionTry =
                httpService.getConnection(Map("host" -> "www.somewhere.com"))

              connectionTry match {
                case Success(_) => fail("Unexpected success")
                case Failure(e) =>
                  e.getMessage shouldBe "Unable to create connection"
              }
            }
          }
          "a request payload is" - {
            val connection = MonadSpec.Connection("www.somewhere.com", "8080")
            "valid" in {
              val responseTry = httpService.issueRequest(connection, "Valid")

              responseTry shouldBe Success("Payload accepted: [Valid]")
            }
            "invalid" in {
              val responseTry = httpService.issueRequest(
                connection,
                "Invalid (as it's much too long)"
              )

              responseTry match {
                case Success(_) => fail("Unexpected success")
                case Failure(e) =>
                  e.getMessage shouldBe "Payload [Invalid (as it's much too long)] isn't valid."
              }
            }
          }
        }
      }
      "a traffic surge protector, when" - {
        "generator a report, when" - {
          "servers are capable" in {
            val result: AsyncResponse[String] = TrafficSurgeDetector
              .generateTrafficSpikeReport("one.example.com", "two.example.com")

            await(result.value) shouldBe Right(
              "Servers [one.example.com] and [two.example.com] can cope."
            )
          }
          "servers are not capable" in {
            val result: AsyncResponse[String] =
              TrafficSurgeDetector.generateTrafficSpikeReport(
                "one.example.com",
                "three.example.com"
              )

            await(result.value) shouldBe Right(
              "Servers [one.example.com] and [three.example.com] *cannot* cope."
            )
          }
          "one of the servers is unrecognised" in {
            val result: AsyncResponse[String] =
              TrafficSurgeDetector.generateTrafficSpikeReport(
                "one.example.com",
                "eleven.example.com"
              )

            await(result.value) shouldBe Left(
              "Error: Server eleven.example.com is unreachable."
            )
          }
          "both of the servers are unrecognised" in {
            val result: AsyncResponse[String] =
              TrafficSurgeDetector.generateTrafficSpikeReport(
                "ten.example.com",
                "eleven.example.com"
              )

            await(result.value) shouldBe Left(
              "Error: Server ten.example.com is unreachable."
            )
          }
        }
      }
    }
  }

  "Demo" in {}
}

object MonadSpec {
  def asPairsList(
      numbers: List[Int],
      chars: List[Char]
  ): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))

  def asPairsOption(
      numberOpt: Option[Int],
      charOpt: Option[Char]
  ): Option[(Int, Char)] =
    numberOpt.flatMap(n => charOpt.map(c => (n, c)))

  def asPairsFuture(numberFut: Future[Int], charFut: Future[Char])(implicit
      executionContext: ExecutionContext
  ): Future[(Int, Char)] =
    numberFut.flatMap(n => charFut.map(c => (n, c)))

  def asPairsGenericOne[M[_], A, B](ma: M[A], mb: M[B])(implicit
      monad: cats.Monad[M]
  ): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  def asPairsGenericTwo[M[_]: cats.Monad, A, B](
      ma: M[A],
      mb: M[B]
  ): M[(A, B)] = {
    import cats.syntax.flatMap._
    import cats.syntax.functor._ // For map
    for {
      a <- ma
      b <- mb
    } yield (a, b)
  }

  case class Connection(host: String, port: String)

  type Config = Map[String, String]
  val config: Config = Map("host" -> "localhost", "port" -> "4040")

  trait HttpService[M[_]] {
    def getConnection(config: Config): M[Connection]

    def issueRequest(connection: Connection, payload: String): M[String]

  }

  object HttpService {

    def getResponse[M[_]](service: HttpService[M], payload: String)(implicit
        monad: cats.Monad[M]
    ): M[String] = {
      import cats.syntax.flatMap._
      import cats.syntax.functor._ // for map
      for {
        connection <- service.getConnection(config)
        response   <- service.issueRequest(connection, payload)
      } yield response
    }

    object HttpServiceUsingOption extends HttpService[Option] {
      override def getConnection(config: Config): Option[Connection] =
        for {
          host <- config.get("host")
          port <- config.get("port")
        } yield Connection(host, port)

      override def issueRequest(
          connection: Connection,
          payload: String
      ): Option[String] =
        if (payload.length < 20) Option(s"Payload accepted: [$payload]")
        else None
    }

    object HttpServiceUsingTry extends HttpService[Try] {
      override def getConnection(config: Config): Try[Connection] = {
        (for {
          host <- config.get("host")
          port <- config.get("port")
        } yield Connection(host, port))
          .map(connection => Success(connection))
          .getOrElse(
            Failure(new RuntimeException("Unable to create connection"))
          )
      }

      override def issueRequest(
          connection: Connection,
          payload: String
      ): Try[String] =
        if (payload.length < 20) Success(s"Payload accepted: [$payload]")
        else
          Failure(
            new IllegalArgumentException(s"Payload [$payload] isn't valid.")
          )
    }

  }

  sealed trait Tree[+A]

  final case class Leaf[+A](a: A) extends Tree[A]

  final case class Branch[+A](l: Tree[A], right: Tree[A]) extends Tree[A]

  object TreeMonad extends Monad[Tree] {
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Leaf(a)      => f(a)
        case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
      }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {

      /*
      TODO: Understand this.

                    _____1_____
                 __2__       __3__
                /     \     /     \
               L1     R2   R3     R4
                tr([1], [], []) =
                tr([3, 2, 1], [1], []) =
                tr([R4, R3, 3, 2, 1], [3, 1], []) =
                tr([R3, 3, 2, 1], [3, 1], [B4]) =
                tr([3, 2, 1], [3, 1], [B3, B4]) =
                tr([2, 1], [1], [B34]) =
                tr([R2, L1, 2, 1], [2, 1], [B34]) =
                tr([L1, 2, 1], [2, 1], [B2, B34]) =
                tr([R1, 2, 1], [2, 1], [B2, B34]) =
                tr([2,1], [2, 1], [B1, B2, B34]) =
                tr([1], [1], [B12, B34]) =
                tr([], [], [B1234]) =
                B1234

       */

      def tailRec(
          todo: List[Tree[Either[A, B]]],
          expanded: Set[Tree[Either[A, B]]],
          done: List[Tree[B]]
      ): Tree[B] = {
        if (todo.isEmpty) done.head
        else
          todo.head match {
            case Leaf(Left(a))  => tailRec(f(a) :: todo.tail, expanded, done)
            case Leaf(Right(b)) => tailRec(todo.tail, expanded, Leaf(b) :: done)
            case node @ Branch(left, right) =>
              if (!expanded.contains(node)) {
                tailRec(right :: left :: todo.tail, expanded + node, done)
              } else {
                val newLeft   = done.head
                val newRight  = done.tail.head
                val newBranch = Branch(newLeft, newRight)
                tailRec(todo.tail, expanded, newBranch :: done.drop(2))
              }
          }
      }

      tailRec(List(f(a)), Set(), List())

    }

    override def pure[A](a: A): Tree[A] = Leaf(a)
  }

  object TrafficSurgeDetector {

    private implicit val ec: ExecutionContext =
      ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

    import cats.data.EitherT
    import cats.instances.future._

    type AsyncResponse[T] = EitherT[Future, String, T]

    private val bandwidths = Map(
      "one.example.com"   -> 50,
      "two.example.com"   -> 300,
      "three.example.com" -> 170
    )

    def generateTrafficSpikeReport(
        serverOne: String,
        serverTwo: String
    ): AsyncResponse[String] =
      canWithstandSurge(serverOne, serverTwo).transform {
        case Right(true) =>
          Right(s"Servers [$serverOne] and [$serverTwo] can cope.")
        case Right(false) =>
          Right(s"Servers [$serverOne] and [$serverTwo] *cannot* cope.")
        case Left(error) => Left(s"Error: $error")
      }

    private def getBandwidth(server: String): AsyncResponse[Int] =
      bandwidths.get(server) match {
        case Some(bandwidth) => EitherT.right(Future(bandwidth))
        case None => EitherT.left(Future(s"Server $server is unreachable."))
      }

    private def canWithstandSurge(
        serverOne: String,
        serverTwo: String
    ): AsyncResponse[Boolean] =
      for {
        bandwidthForServerOne <- getBandwidth(serverOne)
        bandwidthForServerTwo <- getBandwidth(serverTwo)
      } yield (bandwidthForServerOne + bandwidthForServerTwo) > 250

  }

}
