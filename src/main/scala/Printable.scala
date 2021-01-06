import PrintableInstances.printableCat.format
import cats.{Eq, Show}

trait Printable[A] {
  def format(value: A): String
}

object Printable {
  def format[A](value: A)(implicit printer: Printable[A]): String = {
    printer.format(value)
  }

  def print[A](value: A)(implicit printer: Printable[A]): Unit = {
    println(format(value))
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val cat: Cat = Cat("Kitty", 2, "White")
    import ShowInstances.catShow
    import cats.implicits._
    val a = cat.show
    println(a)

  }
}

object PrintableInstances {

  implicit val printableString: Printable[String] = new Printable[String] {
    override def format(value: String): String = {
      value
    }
  }

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
}

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



final case class Cat(name: String, age: Int, colour: String)
