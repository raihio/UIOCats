import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object FoldMapz {
  def foldMap[A, B](ls: Vector[A], f: A => B)(implicit monoid: Monoid[B]): B = {
    ls.map(f).foldLeft(monoid.empty)(monoid.combine)
  }

  def parallelFoldMap[A, B](values: Vector[A])(func: A => B)(implicit monoid: Monoid[B]): Future[B] = {
    import cats.syntax.traverse._
    import cats.instances.future._
    import cats.instances.list._

    val grouped = values.grouped(Runtime.getRuntime.availableProcessors()).toList

    val a: List[Future[B]] = grouped.map(ls => {
      val ec = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())
      Future {
        foldMap(ls, func)
      }(ec)
    })

    import scala.concurrent.ExecutionContext.Implicits.global
    val ls: Future[List[B]] = a.sequence
    ls.map(f => f.foldLeft(monoid.empty)(monoid.combine))
  }

  def main(args: Array[String]): Unit = {
    print(a)
  }
}
