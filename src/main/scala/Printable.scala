import Printable.format
import cats.{Eq, Show}

trait Printable[A] {
  self =>
  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value2: B): String = {
        self.format(func(value2))
      }
    }
}


object Printable {
  def format[A](value: A)(implicit printer: Printable[A]): String = {
    printer.format(value)
  }

  def print[A](value: A)(implicit printer: Printable[A]): Unit = {
    println(format(value))
  }
}


object PrintableInstances {

  implicit val printableInt: Printable[Int] = new Printable[Int] {
    override def format(value: Int): String = {
      value.toString
    }
  }

  implicit val printableCat: Printable[Cat] = new Printable[Cat] {
    override def format(value: Cat): String = {
      s"${value.name} is a ${value.age} year old ${value.colour} cat"
    }
  }

  implicit val stringPrintable: Printable[String] = new Printable[String] {
      def format(value: String): String = s"'${value}'"
  }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
      def format(value: Boolean): String = if(value) "yes" else "no"
  }

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] = {
    p.contramap[Box[A]](value => value.value)
  }
}

object a {
  def main(args: Array[String]): Unit = {
    import PrintableInstances._
    println(format(Box("Hello World!")))
  }
}


final case class Cat(name: String, age: Int, colour: String)
final case class Box[A](value: A)

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit printable: Printable[A]): String = {
      printable.format(value)
    }

    def print(implicit printable: Printable[A]): Unit = {
      println(format(printable))
    }
  }
}

object ShowInstances {
  implicit val catShow: Show[Cat] = (value: Cat) => {
    s"${value.name} is a ${value.age} year old ${value.colour} cat"
  }
}

object CatEquality {
  def main(args: Array[String]): Unit = {

    import cats.instances.string._
    import cats.instances.int._
    import cats.syntax.eq._
    import cats.Eq

    implicit val catEqual: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
        (cat1.name === cat2.name ) &&
          (cat1.age === cat2.age ) &&
          (cat1.colour === cat2.colour)
    }



    val cat1 = Cat("Garfield", 38, "orange and black")
    val cat2 = Cat("Heathcliff", 33, "orange and black")

    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

  }


}
