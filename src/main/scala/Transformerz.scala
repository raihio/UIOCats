import cats.data.EitherT
import cats.implicits.catsStdInstancesForFuture

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Transformerz {

  def main(args: Array[String]): Unit = {
    val a = Future("a")
    val b = Future("b")

    val c: Future[(String, String)] = for {
      a1 <- a
      b1 <- b
    } yield (a1, b1)
  }

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map( "Jazz" -> 6, "Bumblebee" -> 8, "Hot Rod" -> 10)

  def getPowerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot) match {
      case Some(value: Int) => EitherT.right(Future(value))
      case None => EitherT.left(Future(s"$autobot is unreachable!"))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      pl1 <- getPowerLevel(ally1)
      pl2 <- getPowerLevel(ally2)
    } yield (pl1+pl2) > 15
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    Await.result(canSpecialMove(ally1, ally2).value, 1.second) match {
      case Left(value) => s"Comms error: $value"
      case Right(value) =>
        if (value) s"$ally1 and $ally2 are ready to roll out!"
        else s"$ally1 and $ally2 need a recharge."
    }
  }
}
