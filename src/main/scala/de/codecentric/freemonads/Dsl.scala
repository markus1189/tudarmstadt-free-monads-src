package de.codecentric.freemonads

import cats.free.Free
import cats.syntax.foldable._
import cats.instances.option._
import de.codecentric.domain.{Cart, CartId, Item, ItemId}

object Dsl extends Dsl

sealed trait DbAlgebra[A]
case class LoadItem(id: ItemId) extends DbAlgebra[Option[Item]]
case class LoadCart(id: CartId) extends DbAlgebra[Option[Cart]]
case class SaveCart(cart: Cart) extends DbAlgebra[Unit]

trait Dsl {
  type ShopDsl[A] = Free[DbAlgebra, A]

  def loadItem(id: ItemId): ShopDsl[Option[Item]] = Free.liftF(LoadItem(id))

  def loadCart(id: CartId): ShopDsl[Option[Cart]] = Free.liftF(LoadCart(id))

  def saveCart(cart: Cart): ShopDsl[Unit] = Free.liftF(SaveCart(cart))
}

object Programs extends Programs

trait Programs extends Dsl {
  def addItem(iid: ItemId, cid: CartId): ShopDsl[Unit] = for {
    item <- loadItem(iid)
    cart <- loadCart(cid)
    newCart = for {
      i <- item
      c <- cart
    } yield c.add(i)
    r <- newCart.traverse_(saveCart)
  } yield r
}