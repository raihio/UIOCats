import cats.{Applicative, Functor, Monad}
import cats.implicits.toFunctorOps

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object treez {

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  def main(args: Array[String]): Unit = {


  }

  implicit def treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
      tree match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(value) => Leaf(f(value))
      }
    }
  }

  val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def pure[A](a: A): Tree[A] = Leaf(a)

    override def flatMap[A, B](value: Tree[A])(func: A => Tree[B]): Tree[B] = {
      value match {
        case Branch(left, right) => Branch(flatMap(left)(func), flatMap(right)(func))
        case Leaf(value) => func(value)
      }
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      flatMap(f(a)) {
        case Left(value) => tailRecM(value)(f)
        case Right(value) => Leaf(value)
      }
    }
  }
}