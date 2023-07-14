import ReaderSpec.{
  Application,
  Config,
  DbConnection,
  EmailService,
  applicationReader,
  dbConnectionReader,
  emailServiceReader
}
import cats.Id

import scala.annotation.unused

/** Readers are used to construct a data structure based on configuration.
  *
  * There are four steps to the pattern:
  *   1. Create the initial data structure (i.e the "config") 2. Create a Reader
  *      which specifies how that config will be later manipulated. 3. Use map &
  *      flatMap on the Reader to extract the derived values from the config. 4.
  *      When you need the final product (for example, the DbConnection), you
  *      run the Reader with the config.
  *
  * This is the underlying pattern of dependency injection.
  */
class ReaderSpec extends DemoSpec {

  "a reader can construct a data structure based on configuration; for example:" - {

    val config = Config(
      dbUsername = "beau",
      dbPassword = "vine",
      host = "erasmos.com",
      port = 8888,
      emailReplyTo = "trouble@erasmos.com"
    )

    "the DbConnection" in {
      val dbConnection: Id[ReaderSpec.DbConnection] =
        dbConnectionReader.run(config)

      dbConnection shouldBe DbConnection(username = "beau", password = "vine")
    }
    "the EmailService" in {
      val emailService: Id[EmailService] = emailServiceReader.run(config)

      emailService shouldBe EmailService("trouble@erasmos.com")
    }
    "the Application, when" - {
      "emailing a user" in {
        val application: Application = applicationReader.run(config)

        val result = application.emailUser("beau", "beau@sofa.com")

        result shouldBe "From: trouble@erasmos.com; to: beau@sofa.com >>> Your order has the status: [dispatched]"
      }
      "getting the status of the last order" in {
        val application: Application = applicationReader.run(config)

        val result = application.getLastOrderStatus("beau")

        result shouldBe "dispatched"
      }
    }
  }

}

object ReaderSpec {

  import cats.data.Reader

  case class Config(
      dbUsername: String,
      dbPassword: String,
      host: String,
      port: Int,
      emailReplyTo: String
  )

  case class DbConnection(username: String, password: String) {
    def getOrderStatus(@unused orderId: Long): String = "dispatched"

    def getLastOrderId(@unused username: String): Long = 123456
  }

  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String): String =
      s"From: $emailReplyTo; to: $address >>> $contents"
  }

  case class Application(config: Config) {

    def emailUser(username: String, email: String): Id[String] = {
      val reader: Reader[Config, String] =
        for {
          lastOrderId  <- dbConnectionReader.map(_.getLastOrderId(username))
          orderStatus  <- dbConnectionReader.map(_.getOrderStatus(lastOrderId))
          emailService <- emailServiceReader
        } yield emailService.sendEmail(
          email,
          s"Your order has the status: [$orderStatus]"
        )

      reader.run(config)
    }

    def getLastOrderStatus(username: String): Id[String] = {
      val reader: Reader[Config, String] =
        for {
          dbConnection <- dbConnectionReader
          lastOrderId = dbConnection.getLastOrderId(username)
          orderStatus = dbConnection.getOrderStatus(lastOrderId)
        } yield orderStatus
      reader.run(config)
    }
  }

  val applicationReader: Reader[Config, Application] =
    Reader(config => Application(config))

  val dbConnectionReader: Reader[Config, DbConnection] =
    Reader(config => DbConnection(config.dbUsername, config.dbPassword))

  val emailServiceReader: Reader[Config, EmailService] =
    Reader(config => EmailService(config.emailReplyTo))
}
