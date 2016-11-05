package rwalerow.mit6006

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}
import rwalerow.mit6006.AVL.{AvlTree, BSTNode, Empty, Node}

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
    AvlTree.rotateLeft(root)

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

    AvlTree.rotateLeft(root)

    root.key shouldBe 10
    root.left.key shouldBe 8
    root.parent.get.key shouldBe 15
    root.right.key shouldBe 12

    right.key shouldBe 15
    right.parent.isEmpty shouldBe (true)
    right.left.key shouldBe 10
    right.right.isEmpty shouldBe (true)
  }

  it should "rotate in the middle of tree" in {
    val n8 = Node(Empty(), Empty(), 8)
    val n7 = Node(Empty(), n8, 7)
    n8.parent = Some(n7)

    val n10 = Node(n7, Empty(), 10)
    n7.parent = Some(n10)

    val n5 = Node(Empty(), n10, 5)
    n10.parent = Some(n5)

    val byHandTree: BSTNode = n5

    AvlTree.rotateLeft(byHandTree.right.left)

    byHandTree.key shouldBe 5
    byHandTree.right.key shouldBe 10
    byHandTree.right.left.key shouldBe 8
    byHandTree.right.left.left.key shouldBe 7
  }

  "Rotate right" should "rotate basic tree" in new Avls {
    AvlTree.rotateRight(root)

    root.key shouldBe 10
    root.left.isEmpty shouldBe (true)
    root.parent.get.key shouldBe 8

    left.key shouldBe 8
    left.parent.isEmpty shouldBe (true)
    left.right.key shouldBe 10
    left.left.isEmpty shouldBe (true)
  }

  it should "rotate canonical tree" in new Avls {
    avl.insert(9)

    AvlTree.rotateRight(root)

    root.key shouldBe 10
    root.left.key shouldBe 9
    root.parent.get.key shouldBe 8

    left.key shouldBe 8
    left.parent.isEmpty shouldBe (true)
    left.right.key shouldBe 10
    left.left.isEmpty shouldBe (true)
  }

  it should "correctly rotate 2 element tree" in {
    val avl = new AvlTree
    avl.insert(7)
    avl.insert(10)
    val root = avl.root

    AvlTree.rotateLeft(avl.root)

    root.key shouldBe 7
    root.parent.get.key shouldBe 10
  }
}
