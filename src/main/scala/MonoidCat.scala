import cats.Monoid
import cats.syntax.semigroup._
object MonoidCat {
  def main(args: Array[String]): Unit = {
    import cats.instances.int._
    import cats.instances.option._

    val orderMonoid: Monoid[Order] = new Monoid[Order] {
      override def empty: Order = Order(0, 0)

      override def combine(x: Order, y: Order): Order = {
        val totalCost = x.totalCost + y.totalCost
        val totalQuantity = x.quantity + y.quantity
        Order(totalCost, totalQuantity)
      }
    }
    println(add(List(None, Some(2), Some(3))))
  }

  def add[A: Monoid](items: List[A]): A = {
    items.foldLeft(Monoid[A].empty)(_ |+| _)
  }
}

case class Order(totalCost: Double, quantity: Double)
