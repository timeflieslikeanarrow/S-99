package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

//Find out whether a list is palindrome
def isPalindrome[T](list: List[T]): Boolean =
  list == reverse(list)

class P06Tests extends AnyFunSuite:
  test("empty list") {
    assert(isPalindrome(List()))
  }

  test("a single element list") {
    assert(isPalindrome(List(1)))
  }
  test("non palindrome list with more than 1 elements") {
    assert(!isPalindrome(List(1,1,2,3,5,8)))
  }
  
  test("palindrome list with more than 1 element") {
    assert(isPalindrome(List(1,2,3,2,1)))
  }