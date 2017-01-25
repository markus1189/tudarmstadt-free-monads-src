import de.codecentric.domain.{Cart, CartId, Item, ItemId}
import de.codecentric.freemonads._
import de.codecentric.simplealgebra.SimpleAlgebra

import scala.collection.immutable.Seq

val item1 = Item(ItemId("1"), "Lego Deathstar", 40000)

val item2 = Item(ItemId("2"), "Apple", 99)

val item3 = Item(ItemId("3"), "Laptop", 99999)

val items = Seq(item1,item2,item3).groupBy(_.id)

val cart = Cart(CartId("123"), Seq(item1, item2))

Programs.addItem(ItemId("3"), CartId("123"))

SimpleAlgebra.program.foldMap(SimpleAlgebra.interpreter)