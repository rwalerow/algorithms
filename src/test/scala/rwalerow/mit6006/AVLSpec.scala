package rwalerow.mit6006

import org.scalatest.{FlatSpec, Matchers}
import rwalerow.mit6006.AVL._
/**
  * Created by robert on 05.11.16.
  */
class AVLSpec extends FlatSpec with Matchers {

  "AVL" should "correctly create avl tree" in {
    val avl = new AvlTree
    avl.insert(10)

    avl.root.key shouldBe 10
    avl.root.left.isEmpty shouldBe true
    avl.root.right.isEmpty shouldBe true
  }

  it should "create 3 element tree" in {
    val avl = new AvlTree
    avl.insert(10)
    avl.insert(15)
    avl.insert(5)

    avl.root.key shouldBe 10
    avl.root.left.isEmpty shouldBe false
    avl.root.right.isEmpty shouldBe false
    avl.root.left.key shouldBe 5
    avl.root.right.key shouldBe 15
  }

  it should "create 7 element tree" in {
    val avl = new AvlTree
    List(10, 15, 5, 3, 7, 12, 20).foreach(avl.insert(_))

    avl.root.key shouldBe 10
    avl.root.left.isEmpty shouldBe false
    avl.root.left.key shouldBe 5
    avl.root.right.key shouldBe 15
    avl.root.left.left.key shouldBe 3
    avl.root.left.right.key shouldBe 7
    avl.root.right.left.key shouldBe 12
    avl.root.right.right.key shouldBe 20
  }

  "Balance" should "balance this tree" in {
    val avl = new AvlTree
    List(1,2,3,4).foreach(avl.insert(_))

    avl.root.key shouldBe 2
    avl.root.left.key shouldBe 1
    avl.root.right.key shouldBe 3
    avl.root.right.right.key shouldBe 4
  }

  it should "correctly balance twice" in {
    val avl = new AvlTree
    List(5, 10, 3, 7, 8, 15).foreach(avl.insert(_))

    avl.root.key shouldBe 8
    avl.root.left.key shouldBe 5
    avl.root.left.left.key shouldBe 3
    avl.root.left.right.key shouldBe 7
    avl.root.right.key shouldBe 10
    avl.root.right.right.key shouldBe 15
  }

  it should "balance zig zag" in {
    val avl = new AvlTree
    avl.insert(5)
    avl.insert(3)
    avl.insert(4)

    avl.root.key shouldBe 4
    avl.root.left.key shouldBe 3
    avl.root.right.key shouldBe 5
  }

  "Delete" should "preserve balance property" in {
    val avl = new AvlTree
    List(8, 10, 5, 15, 7, 3, 4).foreach(avl.insert(_))

    avl.delete(7)

    avl.root.key shouldBe 8
    avl.root.right.key shouldBe 10
    avl.root.right.right.key shouldBe 15
    avl.root.left.key shouldBe 4
    avl.root.left.left.key shouldBe 3
    avl.root.left.right.key shouldBe 5
  }

  it should "delete from simple tree" in {
    val avl = new AvlTree
    avl.insert(1)
    avl.insert(2)

    avl.delete(2)

    avl.root.key shouldBe 1
    avl.root.right.isEmpty shouldBe true
  }

  it should "delete from middle" in {
    val avl = new AvlTree
    List(4, 5, 3, 6).foreach(avl.insert(_))

    avl.delete(5)

    avl.root.key shouldBe 4
    avl.root.right.key shouldBe 6
  }
}
