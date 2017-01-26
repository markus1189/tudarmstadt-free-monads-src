package de.codecentric.persistence

import com.datastax.driver.core.ResultSet
import de.codecentric.domain.{Cart, Item, ItemId}

object Cassandra {
  def makeItem(rs: ResultSet): Option[Item] = {
    if (rs.getAvailableWithoutFetching == 0) {
      None
    } else {
      val id = ItemId(rs.one.getString("id"))
      val name = rs.one.getString("name")
      val price = rs.one.getLong("price")
      Some(Item(id, name, price))
    }
  }

  def makeCart(rs: ResultSet): Option[Cart] = ???
}
