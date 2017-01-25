package de.codecentric.freemonads

import cats.free.Free
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
