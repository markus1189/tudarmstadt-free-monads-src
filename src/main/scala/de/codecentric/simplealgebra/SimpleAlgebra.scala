package de.codecentric.simplealgebra

import cats.free.Free
import cats.{Id, ~>}

object SimpleAlgebra {
  sealed trait SimpleAlgebra[A]
  case class Plus(x: Int, y: Int) extends SimpleAlgebra[Int]
  case class Mult(x: Int, y: Int) extends SimpleAlgebra[Int]

  def plus(x: Int, y: Int): Free[SimpleAlgebra, Int] =
    Free.liftF(Plus(x,y))

  def mult(x: Int, y: Int): Free[SimpleAlgebra, Int] =
    Free.liftF(Mult(x,y))

  val program: Free[SimpleAlgebra, Int] = for {
    one <- Free.pure(1)
    two <- plus(1,1)
    ten <- Free.pure(10)
    twenty <- mult(ten,two)
    forty <- mult(two,twenty)
    result <- plus(forty,two)
  } yield result

  val interpreter: SimpleAlgebra ~> Id = new (SimpleAlgebra ~> Id) {
    override def apply[A](fa: SimpleAlgebra[A]): Id[A] = fa match {
      case Plus(x,y) => x + y // intellij, why?
      case Mult(x,y) => x * y
    }
  }
}
