package rwalerow.mit6006

/**
  * Created by robert on 03.11.16.
  */
object BST {

  sealed trait BSTNode {

    var parent: Option[BSTNode] = None

    def key: Int
    def left: BSTNode
    def right: BSTNode
    def isEmpty: Boolean
    def setLeft(bSTNode: BSTNode): Unit = ()
    def setRight(bSTNode: BSTNode): Unit = ()
  }

  case class Node(var left: BSTNode, var right: BSTNode, var key: Int) extends BSTNode {
    override def isEmpty: Boolean = false
    override def setLeft(bSTNode: BSTNode): Unit = left = bSTNode
    override def setRight(bSTNode: BSTNode): Unit = right = bSTNode
  }

  case class Empty() extends BSTNode {

    override def left: BSTNode = this
    override def right: BSTNode = this
    override def key: Int = Int.MaxValue
    override def isEmpty: Boolean = true
  }

  def newTree(): BSTNode = Empty()

  def min(root: BSTNode): BSTNode = {
    var curr: BSTNode = root
    while(!curr.left.isEmpty){
      curr = curr.left
    }
    curr
  }

  def max(root: BSTNode): BSTNode = {
    var curr: BSTNode = root
    while(!curr.left.isEmpty){
      curr = curr.right
    }
    curr
  }

  def find(root: BSTNode, key: Int): Option[BSTNode] = {
    if(root.isEmpty) None
    else if(root.key == key) Some(root)
    else if(root.key > key) find(root.left, key)
    else find(root.right, key)
  }

  def insert(root: BSTNode, key: Int): BSTNode = {
    var candidate = root
    var returnValue = root

    while(!candidate.isEmpty && !(candidate.key == key)){
      if(candidate.key < key) candidate = candidate.right
      else candidate = candidate.left
    }

    if(candidate.isEmpty){
      if(candidate.parent.isEmpty){
        returnValue = Node(Empty(), Empty(), key)

        returnValue.left.parent = Some(returnValue)
        returnValue.right.parent = Some(returnValue)
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

    returnValue
  }

  def findNextLarger(root: BSTNode, key: Int): Option[BSTNode] = {
    val startO = find(root, key)

    if(startO.isEmpty) return None

    if(startO.exists(!_.right.isEmpty)) {
      startO.map(node => min(node.right))
    } else {
      var current = startO

      while(current.isDefined && current.flatMap(_.parent).isDefined && current.flatMap(_.parent).exists(_.right eq current.get)){
        current = current.flatMap(_.parent)
      }

      if(current.isEmpty || current.flatMap(_.parent).isEmpty) None
      else current.flatMap(_.parent)
    }
  }

  def findNextSmaller(root: BSTNode, key: Int): Option[BSTNode] = {
    val startO = find(root, key)

    if(startO.isEmpty) return None

    if(startO.exists(!_.left.isEmpty)) {
      startO.map(node => max(node.left))
    } else {
      var current = startO

      while(current.isDefined && current.flatMap(_.parent).isDefined && current.flatMap(_.parent).exists(_.left eq current.get)){
        current = current.flatMap(_.parent)
      }

      if(current.isEmpty || current.flatMap(_.parent).isEmpty) None
      else current.flatMap(_.parent)
    }
  }

  def findNextGeneral(moveUp: BSTNode => BSTNode, moveDown: BSTNode => BSTNode)(root: BSTNode, key: Int): Option[BSTNode] = {
    val startO = find(root, key)

    if(startO.isEmpty) return None

    if(startO.exists(!moveUp(_).isEmpty)) {
      startO.map(moveUp compose moveDown)
    } else {
      var current = startO

      while(current.isDefined && current.flatMap(_.parent).isDefined && current.flatMap(_.parent).exists(moveUp(_) eq current.get)){
        current = current.flatMap(_.parent)
      }

      if(current.isEmpty || current.flatMap(_.parent).isEmpty) None
      else current.flatMap(_.parent)
  }
}
