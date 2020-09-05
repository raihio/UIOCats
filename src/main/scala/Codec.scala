trait Codec[A] { self =>
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))

      override def decode(value: String): B = dec(self.decode(value))
    }
  }
}

object Codec {
  def main(args: Array[String]): Unit = {
    implicit val stringCodec: Codec[String] = new Codec[String] {
      override def encode(value: String): String = value

      override def decode(value: String): String = value
    }

    implicit val doubleCoded: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)

    implicit def boxCodex[A]: Codec[Box[A]] = new Codec[Box[A]] {
      override def encode(value: Box[A]): String =

      override def decode(value: String): Box[A] =
    }
  }
}

def encode[A](value: A)(implicit c: Codec[A]): String =
  c.encode(value)

def decode[A](value: String)(implicit c: Codec[A]): A =
  c.decode(value)

final case class Box[A](value: A)
