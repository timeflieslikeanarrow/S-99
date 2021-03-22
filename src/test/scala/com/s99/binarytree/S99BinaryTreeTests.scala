package com.s99.binarytree

import org.scalatest.funsuite.AnyFunSuite

class S99BinaryTreeTests extends AnyFunSuite:
  test("P56 - Symmetric binary trees") {
    assert(Node('a', Node('b'), Node('c')).isSymmetric === true)
    assert(Node('a', Node('b'), End).isSymmetric === false)
  }
  
  test("P57 - Binary search trees (dictionaries)") {
    val a = End.addValue(2)
    assert(a === Node (2))
    val b = a.addValue(3)
    assert(b === Node (2, End, Node(3)))
    val c = b.addValue(0)
    assert(c === Node(2, Node(0), Node(3)))
  }

  test("P61 - Count the leaves of a binary tree.") {
    assert(Node('x', Node('x'), Node('x')).leafCount === 2)
    assert(Node('x', Node('x'), End).leafCount === 1)
    assert(Node('x', End, End).leafCount === 1)
  }

  test("P62 - Collect the internal nodes of a binary tree in a list") {
    assert(List('a', 'c') === Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList)
    assert(List('a', 'b', 'c') === Node('a', Node('b', Node('f'), End), Node('c', Node('d'), Node('e'))).internalList)
  }

  test("P62B - Collect the nodes at a given level in a list") {
    assert(List('a') === Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(1))
    assert(List('b', 'c') === Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2))
    assert(List('d', 'e') === Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(3))
  }
  