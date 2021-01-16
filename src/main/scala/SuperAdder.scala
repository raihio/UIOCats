import cats.implicits.catsKernelStdGroupForInt

case class Order(totalCost: Double, quantity: Double)

object SuperAdder {
  def main(args: Array[String]): Unit = {

  }

  def add(items: List[Int]): Int = {
    import cats.syntax.semigroup._

    items.head |+| add(items.tail)
  }

  implicit def orderMonad = new Monoid[Order] {
    override def empty: Order = Order(0 ,0)

    override def combine(x: Order, y: Order): Order = {
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
    }
  }
}
