package rwalerow.mit6006

/**
  * Created by robert on 04.11.16.
  */
object AVL {

  class AvlTree {

    import AvlTree._

    var root: BSTNode = Empty()

    def min(node: BSTNode = root): BSTNode = abstractMinMax(_.left)(node)
    def max(node: BSTNode = root): BSTNode = abstractMinMax(_.right)(node)

    def abstractMinMax(sideF: BSTNode => BSTNode)(node: BSTNode) = {
      var curr: BSTNode = node
      while(!sideF(curr).isEmpty) curr = sideF(curr)
      curr
    }

    def find(key: Int, node: BSTNode = root): Option[BSTNode] = {
      if(node.isEmpty) None
      else if(node.key == key) Some(node)
      else if(node.key > key) find(key, node.left)
      else find(key, node.right)
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

    def delete(key: Int): Unit = find(key).foreach(delete)

    def delete(n: BSTNode): Unit = {
      n match {
        case node if node.left.isEmpty && node.right.isEmpty =>
          node.parent.foreach{ p =>
            val newEmpty = Empty()
            newEmpty.parent = Some(p)

            if (p.left eq node) p.setLeft(newEmpty)
            else p.setRight(newEmpty)
          }
        case node if node.left.isEmpty =>
          node.parent.foreach{ p =>
            if (p.left eq node) p.setLeft(node.right)
            else p.setRight(node.right)
          }
          node.right.parent = node.parent
        case node if node.right.isEmpty =>
          node.parent.foreach(p =>
            if (p.left eq node) p.setLeft(node.left)
            else p.setRight(node.left)
          )
        case node =>
          val succ = successor(node, node.key)
          node.setKey(succ.getOrElse(Empty()).key)
          delete(succ.get)
      }
      n.parent.foreach(calculateNodeHeights)
      n.parent.foreach(checkAndCorrect)
    }

    def rightLeftBalance(node: BSTNode): Int = node.right.height - node.left.height
    def calculateNodeHeights(node: BSTNode): Unit = if(!node.isEmpty) node.height = 1 + Math.max(node.left.height, node.right.height)

    def checkAndCorrect(node: BSTNode): Unit = {
      val balance = rightLeftBalance(node)

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
      node.parent.foreach(checkAndCorrect)
    }

    def successor(node: BSTNode, key: Int): Option[BSTNode] = findNextGeneral(_.right, min)(node, key)
    def predecessor(node: BSTNode, key: Int): Option[BSTNode] = findNextGeneral(_.left, max)(node, key)

    def findNextGeneral(moveUp: BSTNode => BSTNode, moveDown: BSTNode => BSTNode)(root: BSTNode, key: Int): Option[BSTNode] = {
      val startO = find(key, root)

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
      genericRotation(_.right, _.left, _.setRight(_), _.setLeft(_))(node)
    }

    def rotateRight(node: BSTNode): Unit = {
      genericRotation(_.left, _.right, _.setLeft(_), _.setRight(_))(node)
    }

    private def genericRotation(getSide: BSTNode => BSTNode,
                        getCounterSide: BSTNode => BSTNode,
                        alignSideSet: (BSTNode, BSTNode) => Unit,
                        counterSideSet: (BSTNode, BSTNode) => Unit)(node: BSTNode) = {
      val sideElement = getSide(node)

      sideElement.parent = node.parent
      node.parent = Some(sideElement)

      alignSideSet(node, getCounterSide(sideElement))
      getSide(sideElement).parent = Some(sideElement)
      counterSideSet(sideElement, node)

      if(sideElement.parent.isDefined){
        if(sideElement.parent.exists(getSide(_) eq node)) sideElement.parent.foreach(alignSideSet(_, sideElement))
        else sideElement.parent.foreach(counterSideSet(_, sideElement))
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
