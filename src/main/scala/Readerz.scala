import cats.data.Reader
import cats.implicits.catsSyntaxApplicativeId

object Readerz {
  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = {
    Reader(db => db.usernames.get(userId))
  }

  def checkPassword(username: String, password: String): DbReader[Boolean] = {
    Reader(db => {
      val pw = db.passwords.get(username)
      pw.isDefined && pw.get.equals(password)
    })
  }

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    for {
      username <- findUsername(userId)
      validPw <- username
        .map(un => checkPassword(un, password))
        .getOrElse(false.pure[DbReader])
    } yield validPw
  }

  def main(args: Array[String]): Unit = {
    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )

    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )
    val db = Db(users, passwords)
    println(checkLogin(1, "zerocool").run(db))
    // res7: cats.package.Id[Boolean] = true
    println(checkLogin(4, "davinci").run(db))
    // res8: cats.package.Id[Boolean] = false

  }
}

final case class Db(usernames: Map[Int, String],
                    passwords: Map[String, String])
