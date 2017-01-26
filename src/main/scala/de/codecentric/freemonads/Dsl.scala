package de.codecentric
package freemonads

import cats.{Id, ~>}
import cats.free.Free
import cats.syntax.foldable._
import cats.instances.option._
import com.datastax.driver.core.{Cluster, Session}
import de.codecentric.domain.{Cart, CartId, Item, ItemId}
import de.codecentric.persistence.Cassandra

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, ExecutionException, Future}

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

trait Interpreter extends Dsl {
  def interpPure(itemDb: mutable.Map[ItemId, Item], cartDb: mutable.Map[CartId, Cart]) =
    new (DbAlgebra ~> Id) {
      override def apply[A](fa: DbAlgebra[A]): Id[A] = fa match {
        case LoadItem(iid) => itemDb.get(iid)
        case SaveCart(cart) =>
          cartDb.put(cart.id, cart)
          ()
        case LoadCart(cid) => cartDb.get(cid)
      }
    }

  def interpEffect(cassandraSession: Session)(implicit ec: ExecutionContext) =
    new (DbAlgebra ~> Future) {
      override def apply[A](fa: DbAlgebra[A]): Future[A] = fa match {
        case LoadItem(iid) =>
          cassandraSession.executeAsync(s"select * from items where id = '${iid.value}'").
            toFuture.map(Cassandra.makeItem)
        case SaveCart(cart) =>
          cassandraSession.executeAsync(
            s"insert into cart (id,itemIds) values (${cart.id.value}," +
              s"${cart.items.map(_.id.value).mkString("[",",","]")}").
            toFuture.map(_ => ())
        case LoadCart(cid) =>
          cassandraSession.executeAsync(s"select * from carts where id = '${cid.value}'").
            toFuture.map(Cassandra.makeCart)
      }
    }
}