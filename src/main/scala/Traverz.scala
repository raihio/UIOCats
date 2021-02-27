
object Traverz {
  def main(args: Array[String]): Unit = {
    val r = List(1, 2, 3, 4, 5).foldRight(List[Int]())(_ :: _)
    val l = List(1, 2, 3, 4, 5).foldLeft(List[Int]().empty)((a, i) => i :: a)

    println(r)
    println(l)

  }
}
