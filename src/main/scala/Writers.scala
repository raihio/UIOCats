import cats.implicits.{catsKernelStdMonoidForVector, catsSyntaxWriterId}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Writers {
  import cats.data.Writer



  def main(args: Array[String]): Unit = {
    val res = Await.result(Future.sequence(Vector(
      Future(factorial(5)),
      Future(factorial(5))
    )), 5.seconds)

    res.foreach(a => println(a.run))
  }

  def slowly[A](body: => A): A = try body finally Thread.sleep(100)


  def factorial(n: Int): Writer[Vector[String], Int] = {
    slowly(
      if (n == 0) Writer(Vector(s"fact $n is 1"), 1)
      else {
        val result = factorial(n - 1)
        val factRes = n * result.value
        Writer(result.written, factRes).tell(Vector(s"fact $n is $factRes"))
    })
  }
}
