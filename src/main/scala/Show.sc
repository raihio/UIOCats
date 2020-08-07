import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats._
import cats.implicits._
val showInt: Show[Int] = Show.apply[Int]
val showString: Show[String] = Show.apply[String]

val intToString: String = showInt.show(123)


import java.util.Date
implicit val dateShow: Show[Date] = Show.show(date => s"${date.getTime} ms since the epoch!")

case class Cat(name: String, age: Int, colour: String)


implicit val catShow: Show[Cat] = Show.show(cat => {
  s"${cat.name.show} is a ${cat.age.show} year old ${cat.colour.show} cat!"
})

println(Cat("Zinger", 5, "Ginger").show)