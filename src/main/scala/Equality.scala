import cats.Eq
import cats.syntax.eq._
import cats.instances.string._
import cats.instances.int._

object Equality {

  implicit val catEq: Eq[Cat] = {
    Eq.instance[Cat] {(cat1, cat2) => {
      (cat1.name === cat2.name) && (cat2.colour === cat1.colour) && (cat1.age === cat2.age)
    }}
  }

}
