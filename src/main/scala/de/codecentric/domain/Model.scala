package de.codecentric.domain

import scala.collection.immutable.Seq

case class ItemId(value: String)
case class Item(id: ItemId, name: String, price: Long)

case class CartId(value: String)
case class Cart(id: CartId, items: Seq[Item]) {

  def add(item: Item): Cart = Cart(id, items :+ item)

  def total: Long = items.foldLeft(0l)(_ + _.price)

}