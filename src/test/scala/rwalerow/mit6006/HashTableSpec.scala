package rwalerow.mit6006

import org.scalatest.{Matchers, WordSpec}
import HashTable._
/**
  * Created by robert on 27.11.16.
  */
class HashTableSpec extends WordSpec with Matchers {

  "Open addressing" should {

    "work without resizing" in {
      val table = new OAHashTable
      table.insert("robert")
      table.insert("aneta")
      table.insert("kot")

      table.contains("robert") shouldBe true
      table.contains("aneta") shouldBe true
      table.contains("kot") shouldBe true
      table.contains("kikula") shouldBe false
    }

    "grow properly" in {
      val table = new OAHashTable
      val words = List("ala", "ma", "kota", "a", "ten", "zwiarz", "nie", "do", "konca", "i", "ja")
      words.foreach(table.insert)

      words.foreach { word =>
        table.contains(word) shouldBe true
      }
    }

    "search value in table prerly" in {
      val table = new OAHashTable
      table.insert("robert")
      table.insert("aneta")

      table.search("aneta") shouldBe true
    }

    "add and delete properly" in {
      val table = new OAHashTable
      table.insert("ala")
      table.insert("ma")

      table.delete("ala")

      table.contains("ala") shouldBe false
      table.contains("ma") shouldBe true
      table.length shouldBe 1
    }

    "insert -> delete -> find one after" in {
      val table = new OAHashTable
      table.insert("robert")
      table.insert("adami")

      table.delete("robert")

      table.search("robert") shouldBe false
      table.search("adami") shouldBe true
    }

    "insert -> delete -> insert and not contain DeleteMe" in {
      val table = new OAHashTable
      table.insert("robert")
      table.insert("adami")

      table.delete("robert")
      table.insert("robert")

      table.contains("robert") shouldBe true
      table.contains("adami") shouldBe true
    }

    "shrink table on delete" in {
      val table = new OAHashTable {
        def innerSize() = size
      }

      val words = List("ala", "ma", "kota", "a", "ten", "zwiarz", "nie", "do", "konca", "i", "ja")
      words.foreach(table.insert)

      table.delete("i")
      table.delete("ja")
      table.delete("do")
      table.delete("a")
      table.delete("nie")
      table.delete("ala")

      table.innerSize() shouldBe 10

      table.search("ma") shouldBe true
      table.search("kota") shouldBe true
    }
  }
}
