package rwalerow.mit6006

import rwalerow.mit6006.BST.{Empty, Node}

/**
  * Created by robert on 04.11.16.
  */
object AVL {

  class AvlTree {
    var root: BSTNode = Empty()

    def min(node: BSTNode = root): BSTNode = {
      var curr: BSTNode = node
      while(!curr.left.isEmpty){
        curr = curr.left
      }
      curr
    }

    def max(node: BSTNode = root): BSTNode = {
      var curr: BSTNode = node
      while(!curr.left.isEmpty){
        curr = curr.right
      }
      curr
    }

    def find(node: BSTNode = root, key: Int): Option[BSTNode] = {
      if(node.isEmpty) None
      else if(node.key == key) Some(node)
      else if(node.key > key) find(node.left, key)
      else find(node.right, key)
    }

    def insert(key: Int, node: BSTNode = root): Unit = {
      var candidate = node

      while(!candidate.isEmpty && !(candidate.key == key)){
        if(candidate.key < key) candidate = candidate.right
        else candidate = candidate.left
      }

      if(candidate.isEmpty){
        if(candidate.parent.isEmpty){
          root = Node(Empty(), Empty(), key)

          root.left.parent = Some(root)
          root.right.parent = Some(root)
        } else {
          val candidateParent = candidate.parent.get
          val newInsert = Node(Empty(), Empty(), key)

          newInsert.left.parent = Some(newInsert)
          newInsert.right.parent = Some(newInsert)
          newInsert.parent = candidate.parent

          if (candidateParent.left eq candidate) {
            candidateParent.setLeft(newInsert)
          } else {
            candidateParent.setRight(newInsert)
          }
        }
      }
    }

    def delete(node: BSTNode): Unit = {
      if(node.left.isEmpty && node.right.isEmpty){
        node.parent.foreach(p => {
          val newEmpty = Empty()
          newEmpty.parent = Some(p)

          if(p.left eq node) p.setLeft(newEmpty)
          else p.setRight(newEmpty)
        })
      } else if(node.left.isEmpty) {
        node.parent.foreach { p =>
          if (p.left eq node) p.setLeft(node.right)
          else p.setRight(node.right)
        }
        node.right.parent = node.parent
      } else if(node.right.isEmpty) {
        node.parent.foreach(p =>
          if (p.left eq node) p.setLeft(node.left)
          else p.setRight(node.left)
        )
      } else {
        val succ = successor(node, node.key)
        node.setKey(succ.getOrElse(Empty()).key)
        delete(succ.get)
      }
    }

    def successor(node: BSTNode, key: Int): Option[BSTNode] = findNextGeneral(_.right, min)(node, key)
    def predecessor(node: BSTNode, key: Int): Option[BSTNode] = findNextGeneral(_.left, max)(node, key)

    def findNextGeneral(moveUp: BSTNode => BSTNode, moveDown: BSTNode => BSTNode)(root: BSTNode, key: Int): Option[BSTNode] = {
      val startO = find(root, key)

      if(startO.isEmpty) return None

      if(startO.exists(!moveUp(_).isEmpty)) {
        startO.map(moveDown compose moveUp)
      } else {
        var current = startO

        while(current.isDefined && current.flatMap(_.parent).isDefined && current.flatMap(_.parent).exists(moveUp(_) eq current.get)){
          current = current.flatMap(_.parent)
        }

        if(current.isEmpty || current.flatMap(_.parent).isEmpty) None
        else current.flatMap(_.parent)
      }
    }

    def rotateLeft(node: BSTNode) = {
      val right = node.right

      right.parent = node.parent
      node.parent = Some(right)

      node.setRight(right.left)
      node.right.parent = Some(node)
      right.setLeft(node)
    }

    def rotateRight(node: BSTNode) = {
      val left = node.left

      left.parent = node.parent
      node.parent = Some(left)

      node.setLeft(left.right)
      node.left.parent = Some(node)
      left.setRight(node)
    }
  }

  sealed trait BSTNode {

    var parent: Option[BSTNode] = None

    def key: Int
    def left: BSTNode
    def right: BSTNode
    def isEmpty: Boolean
    def setLeft(bSTNode: BSTNode): Unit = ()
    def setRight(bSTNode: BSTNode): Unit = ()
    def setKey(k: Int): Unit
  }

  case class Node(var left: BSTNode, var right: BSTNode, var key: Int) extends BSTNode {
    override def isEmpty: Boolean = false
    override def setLeft(bSTNode: BSTNode): Unit = left = bSTNode
    override def setRight(bSTNode: BSTNode): Unit = right = bSTNode
    override def setKey(k: Int): Unit = key = k
  }

  case class Empty() extends BSTNode {
    override def left: BSTNode = this
    override def right: BSTNode = this
    override def key: Int = Int.MaxValue
    override def isEmpty: Boolean = true
    override def setKey(k: Int): Unit = ()
  }
}
