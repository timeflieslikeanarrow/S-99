package com.s99.binarytree

sealed abstract class Tree[+T]:
  def isMirrofOf[U >: T](tree: Tree[U]): Boolean
  def isSymmetric: Boolean
  def leafCount: Int = 0
  def internalList: List[T] = List()
  def atLevel(level: Int): List[T] = List()
  def addValue[U >: T](value:U)(using ordering: Ordering[U]): Tree[U] = Node(value)

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
}

case object End extends Tree[Nothing] {
  override def isMirrofOf[T](tree: Tree[T]): Boolean = tree match
    case End => true
    case _ => false
  
  override def isSymmetric: Boolean = true
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}
