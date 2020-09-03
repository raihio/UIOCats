object PrintableContramap {
  def main(args: Array[String]): Unit = {
    implicit val stringPrintable: Printable[String] =
      new Printable[String] {
        def format(value: String): String =
          s"'${value}'"
      }
    implicit val booleanPrintable: Printable[Boolean] =
      new Printable[Boolean] {
        def format(value: Boolean): String =
          if(value) "yes" else "no"
      }
    format("hello")
    format(true)
  }
}


trait Printable[A] { self =>
  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String =
        self.format(func(value))
    }
}
def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)


implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] = p.contramap[Box[A]](_.value)

final case class Box[A](value: A)
