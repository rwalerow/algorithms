package rwalerow.mit6006
import rwalerow.mit6006.HashingFunctions._
/**
  * Created by robert on 15.11.16.
  */
object HashTable {

  trait ArrayElement
  object DeleteMe extends ArrayElement
  object Empty extends ArrayElement
  case class Value(v: String) extends ArrayElement

  type OAHashTable = OpenAddressingHashTable

  class OpenAddressingHashTable {
    protected var size = 10
    protected var inserted = 0
    protected var table: Array[ArrayElement] = Array.fill(10){ Empty }

    def length = inserted

    def insert(value: String): Unit =
      if(size > inserted) insert(value, table)
      else { extendTable(); insert(value, table) }

    def delete(value: String): Unit =
      if((size / 3) < inserted) innerDelete(value)
      else { innerDelete(value); shrinkTable() }

    private def insert(value: String, insertTable: Array[ArrayElement] = table): Unit = {
      val cIndex = candidateIndex(value, c => !List(Empty, DeleteMe).contains(insertTable(c)), insertTable)
      inserted += 1
      insertTable(cIndex) = Value(value)
    }

    def search(value: String): Boolean = {
      val candidateInedex = candidateIndex(value, x => table(x) == Empty || table(x) == DeleteMe)
      table(candidateInedex) == Value(value)
    }

    private def innerDelete(value: String): Unit = {
      val cIndex = candidateIndex(value, x => table(x) == Empty || table(x) != Value(value))
      if(table(cIndex) == Value(value)) {
        inserted -= 1
        table(cIndex) = DeleteMe
      }
    }

    private def candidateIndex(value: String, loopF: Int => Boolean, tab: Array[ArrayElement] = table): Int = {
      var i = 0
      var candidate = hashWithIteration(str = value, i = i) % tab.length
      while(loopF(candidate)) {
        i += 1
        candidate = hashWithIteration(str = value, i = i) % tab.length
      }
      candidate
    }

    def contains(value: String): Boolean =
      table.exists {
        case Value(v) => v == value
        case _ => false
      }

    private def extendTable(): Unit = {
      val startInserted = inserted
      val newTable: Array[ArrayElement] = Array.fill(size * 2){ Empty }
      table.foreach {
        case Value(v) => insert(v, newTable)
        case _ => ()
      }
      size *= 2
      table = newTable
      inserted = startInserted
    }

    private def shrinkTable(): Unit = {
      val startInserted = inserted
      val newTable: Array[ArrayElement] = Array.fill(size / 2){ Empty }
      table.foreach {
        case Value(v) => insert(v, newTable)
        case _ => ()
      }
      size /= 2
      table = newTable
      inserted = startInserted
    }
  }
}
