package rwalerow.mit6006

/**
  * Created by robert on 04.11.16.
  */
object AVL {

  class AvlTree {

    import AvlTree._

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
          newInsert.height = 0
          newInsert.parent = candidate.parent

          if (candidateParent.left eq candidate) {
            candidateParent.setLeft(newInsert)
          } else {
            candidateParent.setRight(newInsert)
          }
        }
      }

      candidate.parent.foreach(checkAndCorrect)
    }

    def delete(n: BSTNode): Unit = n match {
      case node if node.left.isEmpty && node.right.isEmpty => {
        node.parent.foreach(p => {
          val newEmpty = Empty()
          newEmpty.parent = Some(p)

          if(p.left eq node) p.setLeft(newEmpty)
          else p.setRight(newEmpty)
        })
      }
      case node if node.left.isEmpty => {
        node.parent.foreach { p =>
          if (p.left eq node) p.setLeft(node.right)
          else p.setRight(node.right)
        }
        node.right.parent = node.parent
      }
      case node if node.right.isEmpty => {
        node.parent.foreach(p =>
          if (p.left eq node) p.setLeft(node.left)
          else p.setRight(node.left)
        )
      }
      case node => {
        val succ = successor(node, node.key)
        node.setKey(succ.getOrElse(Empty()).key)
        delete(succ.get)
      }
    }

    def rightLeftBalance(node: BSTNode): Int = node.right.height - node.left.height
    def calculateNodeHeights(node: BSTNode): Unit = if(!node.isEmpty) node.height = 1 + Math.max(node.left.height, node.right.height)

    def checkAndCorrect(node: BSTNode): Unit = {
      val balance = rightLeftBalance(node)
      val next = node.parent

      if(balance > 1){
        val rightBalance = rightLeftBalance(node.right)
        if(rightBalance >= 0){
          rotateLeft(node)
        } else {
          rotateRight(node.right)
          rotateLeft(node)
          node.parent.foreach(calculateNodeHeights)
          node.parent.map(_.right).foreach(calculateNodeHeights)
        }
      } else if(balance < -1){
        val leftBalance = rightLeftBalance(node.left)
        if(leftBalance <= 0){
          rotateRight(node)
        } else {
          rotateLeft(node.left)
          rotateRight(node)
          node.parent.foreach(calculateNodeHeights)
          node.parent.map(_.left).foreach(calculateNodeHeights)
        }
      }

      if(Math.abs(balance) > 1 && (node eq root)) root = root.parent.get
      calculateNodeHeights(node)
      next.foreach(checkAndCorrect)
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
  }

  object AvlTree {
    def rotateLeft(node: BSTNode): Unit = {
      val right = node.right

      right.parent = node.parent
      node.parent = Some(right)

      node.setRight(right.left)
      node.right.parent = Some(node)
      right.setLeft(node)

      if(right.parent.isDefined){
        if(right.parent.exists(_.left eq node)) right.parent.foreach(_.setLeft(right))
        else right.parent.foreach(_.setRight(right))
      }
    }

    def rotateRight(node: BSTNode): Unit = {
      val left = node.left

      left.parent = node.parent
      node.parent = Some(left)

      node.setLeft(left.right)
      node.left.parent = Some(node)
      left.setRight(node)

      if(left.parent.isDefined){
        if(left.parent.exists(_.left eq node)) left.parent.foreach(_.setLeft(left))
        else left.parent.foreach(_.setRight(left))
      }
    }
  }

  sealed trait BSTNode {

    var parent: Option[BSTNode] = None
    var height = -1

    def key: Int
    def left: BSTNode
    def right: BSTNode
    def isEmpty: Boolean
    def setLeft(bSTNode: BSTNode): Unit = ()
    def setRight(bSTNode: BSTNode): Unit = ()
    def setKey(k: Int): Unit = ()
  }

  case class Node(
                   var left: BSTNode,
                   var right: BSTNode,
                   var key: Int) extends BSTNode {

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
  }
}
