trait Codec[A] {
  self =>
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))

      override def decode(value: String): B = dec(self.decode(value))
    }
  }
}


object codecEx {
  def main(args: Array[String]): Unit = {
    implicit val doubleCodec: Codec[Double] = new Codec[Double] {
      override def encode(value: Double): String = value.toString
      override def decode(value: String): Double = value.toDouble
    }
  }

  implicit val stringCodec: Codec[String] = new Codec[String] {
      def encode(value: String): String = value
      def decode(value: String): String = value
    }


  implicit def boxCodec[T](implicit tCodec: Codec[T]): Codec[Box[T]] = {
    tCodec.imap[Box[T]](Box(_), _.value)
  }
}

