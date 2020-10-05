import cats.Id

trait Monad[F[_]] {
  def pure[A](value: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] = {
    flatMap(value)(a => pure(func(a)))
  }
}

class IdMonda {
  def pure[A](value: A): Id[A] = {
    value
  }

  def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = {
    func(value)
  }

  def map[A, B](value: Id[A])(func: A => B): Id[B] = {
    func(value)
  }
}