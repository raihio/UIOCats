trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) =
    monoid
}

object BooleansMonoids {

  // Boolean Monoids
  implicit val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = {
      x && y
    }
  }

  implicit val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = {
      x || y
    }
  }

  implicit val booleanXor: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = {
      (x && !y) || (!x && y)
    }
  }

  implicit val booleanNor: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = {
      (x || !y) && (!x || y)
    }
  }
}

object setMonoids {
  // Sets Monoids and Semigroups
  implicit def setUnion[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty

    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  // It's not a Monoid because we do not have an indentity element
  // If you intersect a Set[String] with Set.empty, you get an empty set
  implicit def setIntersection[A]: Semigroup[Set[A]] = new Semigroup[Set[A]] {
    override def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y
  }

  // Similar to Set XOR
  implicit def setSymDiff[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty

    override def combine(x: Set[A], y: Set[A]): Set[A] = {
      (x union y) -- (x intersect y)
    }
  }

  def main(args: Array[String]): Unit = {

  }
}