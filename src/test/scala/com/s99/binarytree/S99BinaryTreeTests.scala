package com.s99.binarytree

import org.scalatest.funsuite.AnyFunSuite

class S99BinaryTreeTests extends AnyFunSuite:
  test("P56 - Symmetric binary trees") {
    assert(Node('a', Node('b'), Node('c')).isSymmetric === true)
    assert(Node('a', Node('b'), End).isSymmetric === false)
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
  