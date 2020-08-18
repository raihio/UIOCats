object FunctorExamples {
  def main(args: Array[String]): Unit = {

    import cats.instances.function._
    import cats.syntax.functor._

    val func1: Int => Double =
      (x: Int) => x.toDouble

    val func2: Double => Double =
      (y: Double) => y * 2

    println((func1 map func2)(1))
    println((func1 andThen func2)(1))
    println(func2(func1(1)))
  }

}
