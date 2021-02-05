import Statez.CalcState
import cats.data.State
import cats.implicits.catsSyntaxApplicativeId

import scala.::
import scala.collection.IterableOnce

object Statez {

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = {
    State[List[Int], Int] { oldStack: List[Int] =>
      val newStack: List[Int] = {
        sym match {
          case "+" => oldStack.tail.tail
          case "-" => oldStack.tail.tail
          case "*" => oldStack.tail.tail
          case "/" => oldStack.tail.tail
          case _ => oldStack
        }
      }
      val result = {


        sym match {
          case "+" => {
            val a = oldStack.head
            val b = oldStack.tail.head
            a + b
          }
          case "-" => {
            val a = oldStack.head
            val b = oldStack.tail.head
            a - b
          }
          case "*" => {
            val a = oldStack.head
            val b = oldStack.tail.head
            a * b
          }
          case "/" => {
            val a = oldStack.head
            val b = oldStack.tail.head
            a / b
          }
          case num => sym.toInt
        }
      }
      (result::newStack, result)
    }
  }

  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(0.pure[CalcState]) { (a, b) =>
      a.flatMap(_ => evalOne(b))
    }
  }

  def evalInput(s: String): CalcState[Int] = {
    val ls = s.split("").toList
    evalAll(ls)
  }

  def main(args: Array[String]): Unit = {

    val biggerProgram = for {
      _ <- evalAll(List("1", "2", "+"))
      _ <- evalAll(List("3", "4", "+"))
      ans <- evalOne("*")
    } yield ans

    println(biggerProgram.runA(Nil).value)

  }
}
