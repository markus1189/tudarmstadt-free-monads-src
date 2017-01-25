import cats.{Id, ~>}
import de.codecentric.domain.{Cart, CartId, Item, ItemId}
import de.codecentric.freemonads._
import de.codecentric.simplealgebra.SimpleAlgebra

import scala.collection.immutable.Seq
import scala.collection.mutable

val item1 = Item(ItemId("1"), "Lego Deathstar", 40000)

val item2 = Item(ItemId("2"), "Apple", 99)

val item3 = Item(ItemId("3"), "Laptop", 99999)

val items = Seq(item1,item2,item3).groupBy(_.id).mapValues(_.head)

val cart = Cart(CartId("123"), Seq(item1, item2))

val p = Programs.addItem(ItemId("3"), CartId("123"))

SimpleAlgebra.program.foldMap(SimpleAlgebra.interpreter)

def interpPure(itemDb: mutable.Map[ItemId,Item], cartDb: mutable.Map[CartId,Cart]) =
  new (DbAlgebra ~> Id) {
    override def apply[A](fa: DbAlgebra[A]): Id[A] = fa match {
      case LoadItem(iid) => itemDb.get(iid)
      case SaveCart(cart) =>
        cartDb.put(cart.id, cart)
        ()
      case LoadCart(cid) => cartDb.get(cid)
    }
  }

val itemsDb = mutable.Map(items.toList:_*)
val cartsDb = mutable.Map(cart.id -> cart)
p.foldMap(interpPure(itemsDb, cartsDb))
itemsDb
cartsDb