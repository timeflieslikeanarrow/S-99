package com.s99.binarytree

sealed abstract class Tree[+T]:
  def isMirrofOf[U >: T](tree: Tree[U]): Boolean
  def isSymmetric: Boolean
  def leafCount: Int = 0
  def internalList: List[T] = List()
  def atLevel(level: Int): List[T] = List()
  def addValue[U >: T](value:U)(using ordering: Ordering[U]): Tree[U] = Node(value)
  def toString2: String

object Tree:
  def findSeparator(s: String): Int = {
    var leftParentCount = 0
    var commaCount = 0
    for (i <- 0 until s.length) {
      if (s(i) == ',') {
        if (leftParentCount == 0) return i
        else commaCount += 1
      } else if (s(i) == '(') {
        leftParentCount += 1
      } else if (s(i) == ')') {
        leftParentCount -= 1
        if (commaCount > 0)
          commaCount -= 1
      }
    }

    s.length
  }

  def fromString(s: String): Tree[Char] = {
    if (s.length == 0) End
    else if (s.length == 1) Node(s(0))
    else {
      val value = s(0)
      val rest = s.dropRight(1).drop(2)
      val separatorIndex = findSeparator(rest)
      val left = rest.substring(0, separatorIndex)
      val right = rest.substring(separatorIndex + 1)
      Node(value, fromString(left), fromString(right))
    }
  }

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def isMirrofOf[U >: T](tree: Tree[U]): Boolean =
    tree match {
      case End => false
      case Node(_, l, r) => left.isMirrofOf(l) && right.isMirrofOf(r)
    }

  override def leafCount: Int = (left, right) match
    case (End, End) => 1
    case _ => left.leafCount + right.leafCount

  override def internalList: List[T] = (left, right) match {
    case (End, End) => List()
    case _ => value +: (left.internalList ++ right.internalList)
  }

  override def atLevel(level: Int): List[T] = 
    require(level >= 0)
    if (level == 1) List(value)
    else left.atLevel(level - 1) ++ right.atLevel(level - 1)

  override def addValue[U >: T](value: U)(using ordering: Ordering[U]) : Tree[U] =
    if value == this.value then this
    else if ordering.compare(value,this.value) < 0 then 
      Node(this.value, left.addValue(value), right)
    else Node(this.value, left, right.addValue(value))

  override def isSymmetric: Boolean = left.isMirrofOf(right)
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  override def toString2: String = {
    if (left == End && right == End) s"$value"
    else s"$value(${left.toString2},${right.toString2})"
  }
}

case object End extends Tree[Nothing] {
  override def isMirrofOf[T](tree: Tree[T]): Boolean = tree match
    case End => true
    case _ => false
  
  override def isSymmetric: Boolean = true
  override def toString = "."

  override def toString2: String = ""
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}
