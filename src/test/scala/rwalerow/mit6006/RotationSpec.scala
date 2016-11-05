package rwalerow.mit6006

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}
import rwalerow.mit6006.AVL.AvlTree

/**
  * Created by robert on 05.11.16.
  */
class RotationSpec extends FlatSpec with Matchers {

  trait Avls {
    val avl = new AvlTree
    avl.insert(10)
    avl.insert(15)
    avl.insert(8)

    val root = avl.root
    val right = root.right
    val left = root.left
  }

  "Rotate left" should "rotate basic tree" in new Avls {
    avl.rotateLeft(root)

    root.key shouldBe 10
    root.left.key shouldBe 8
    root.parent.get.key shouldBe 15

    right.key shouldBe 15
    right.parent.isEmpty shouldBe (true)
    right.left.key shouldBe 10
    right.right.isEmpty shouldBe (true)
  }

  it should "rotate more canonical example" in new Avls {
    avl.insert(12)

    avl.rotateLeft(root)

    root.key shouldBe 10
    root.left.key shouldBe 8
    root.parent.get.key shouldBe 15
    root.right.key shouldBe 12

    right.key shouldBe 15
    right.parent.isEmpty shouldBe (true)
    right.left.key shouldBe 10
    right.right.isEmpty shouldBe (true)
  }

  "Rotate right" should "rotate basic tree" in new Avls {
    avl.rotateRight(root)

    root.key shouldBe 10
    root.left.isEmpty shouldBe (true)
    root.parent.get.key shouldBe 8

    left.key shouldBe 8
    left.parent.isEmpty shouldBe (true)
    left.right.key shouldBe 10
    left.left.isEmpty shouldBe (true)
  }
}
