trait Printable[A] {
  def format(a: A): String
}

object PrintableInstances {

  implicit val printableString: Printable[String] = (a: String) => {
    a
  }

  implicit val printableInt: Printable[Int] = (a: Int) => {
    a.toString
  }

  implicit val printableCat: Printable[Cat] = new Printable[Cat] {
    override def format(cat: Cat): String = {
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val colour = Printable.format(cat.colour)
      s"$name is a $age year old $colour cat."
    }
  }

  def main(args: Array[String]): Unit = {
    val kiddy: Cat = Cat("Kiddy", 5, "white")
    Printable.print(kiddy)
  }
}

object PrintableSyntax {
  implicit class PrintableOps[A](a: A) {
    def format(implicit printable: Printable[A]): String = {
      printable.format(a)
    }

    def print(implicit printable: Printable[A]): Unit = {
      println(format(printable))
    }
  }

  def main(args: Array[String]): Unit = {
    val zinger: Cat = Cat("Zinger", 5, "ginger")
    val pOps: PrintableOps[Cat] = new PrintableOps[Cat](zinger)

    import PrintableInstances.printableCat
    pOps.print
  }
}

object Printable {
   def format[A](value: A)(implicit printable: Printable[A]): String = {
     printable.format(value)
   }
  def print[A](value: A)(implicit printable: Printable[A]): Unit = {
    println(format(value))
  }
}
