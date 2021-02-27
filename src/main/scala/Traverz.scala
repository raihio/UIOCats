import cats.Foldable
import cats.implicits.{catsKernelStdGroupForInt, catsStdInstancesForList, catsStdInstancesForVector}

object Traverz {
  def main(args: Array[String]): Unit = {

    val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))

    val b = (Foldable[List] compose Foldable[Vector]).combineAllOption(ints)
    println(b)
  }

  def map[A, B](ls: List[A])(fn: A => B): List[B] = {
    ls.foldRight(List.empty[B])((a, i) => fn(a) :: i)
  }

  def flatMap[A, B](list: List[A])(func: A => List[B]): List[B] = {
    list.foldRight(List.empty[B])((item, acc) => func(item) ::: acc)
  }

  def filter[A](list: List[A])(func: A => Boolean): List[A] = {
    list.foldRight(List.empty[A])((item, acc) => {
      if (func(item)) item :: acc
      else acc
    })

  }


}
