import cats.Functor
import cats.implicits.toFunctorOps

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object treez {
  def main(args: Array[String]): Unit = {
    val intTree: Tree[Int] = Branch(Leaf(10), Leaf(20))
    val a = intTree.map(_ * 2)
    println(a)
  }

  implicit def treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
      tree match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(value) => Leaf(f(value))
      }
    }
  }
}