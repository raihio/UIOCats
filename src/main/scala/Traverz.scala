import cats.Applicative
import cats.implicits.{catsStdInstancesForOption, catsStdInstancesForVector, catsSyntaxApplicativeId}
import cats.syntax.apply._ // for mapN

object Traverz {
  def main(args: Array[String]): Unit = {
    val vec = List(Vector(1, 2), Vector(3, 4))
    val ab = listSequence(vec)

    // Combining accumulator and hostname using an Applicative:
    process(List(2, 4, 6))
    process(List(1, 2, 3))


  }

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) {
      (accum: F[List[B]], item: A) => {
        (accum, func(item)).mapN((value: List[B], b: B) => {
          value :+ b
        })
      }
    }

  def listSequence[F[_]: Applicative, B] (list: List[F[B]]): F[List[B]] = listTraverse(list)(identity)

  def process(inputs: List[Int]): Option[List[Int]] = {
    listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)
  }
}
