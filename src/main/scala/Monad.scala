trait Monad[F[_]] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
  def map[A, B](value: F[A])(func: A => B): F[B] = {
   flatMap(value)((a: A) => {
     pure(a)
   })
  }
}

import cats.{Eval, Id, MonadError}

import scala.util.Try

class IdMonad extends Monad[Id] {
  override def pure[A](a: A): Id[A] = a

  override def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = {
    func(value)
  }

  override def map[A, B](value: Id[A])(func: A => B): Id[B] = {
    func(value)
  }
}

def main(args: Array[String]): Unit = {
  import cats.syntax.applicative._
  import cats.syntax.monadError._
  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] = {
    val validatedAge = age.pure[F]
    validatedAge.ensure(new IllegalArgumentException("Age must be 18 or greater"))(_ >= 18)
  }


  def foldRight[A, B](as: List[A], acc: B)(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail => Eval.defer(fn(head, foldRight(tail, acc)(fn)))
      case Nil => Eval.now(acc)
    }

}