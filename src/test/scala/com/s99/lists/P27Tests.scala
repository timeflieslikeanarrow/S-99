package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

import java.util.NoSuchElementException

//Group the elements of a set into disjoint subsets.
def group3[T](list: List[T]): List[List[List[T]]] =
  val result = scala.collection.mutable.Set[List[Set[T]]]()

  def iter(current: List[T], rest: List[T]): Unit = {
    if current.length == list.length then 
      result += List(current.take(2).toSet, current.drop(2).take(3).toSet, current.drop(5).toSet)
    else
      for i <- 0 until rest.length do
        val (remaining, item) = removeAt(i, rest)
        iter(item::current, remaining)
  }

  if list.length != 9 then throw IllegalArgumentException("the list length must be 9")
  else {
    iter(List(), list)
    result.map { case list => list.map(_.toList)}.toList
  }

//Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the **>same(?)<*** solution as ((Beat, Aldo), ...). 
//However, we make a difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).
def group[T](counts: List[Int], list: List[T]): List[List[List[T]]] =
  val result = scala.collection.mutable.Set[List[Set[T]]]()

  def iter(current: List[T], rest: List[T]): Unit = {
    if current.length == list.length then
      result += List(current.take(counts(0)).toSet, current.drop(counts(0)).take(counts(1)).toSet, current.drop(counts(0) + counts(1)).toSet)
    else
      for i <- 0 until rest.length do
        val (remaining, item) = removeAt(i, rest)
        iter(item::current, remaining)
  }

  if list.length != 9 || counts.length != 3 || counts.sum != 9 then throw IllegalArgumentException("the list length must be 9")
  else {
    iter(List(), list)
    result.map { case list => list.map(_.toList)}.toList
  }

class P27Tests extends AnyFunSuite:
  test("group3 => c(9,2)*c(7,3)*c(4,4)==1260") {
    val result = group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
    println(result)
    assert(1260 === result.size)
  }

  test("group => c(9,2)*c(7,2)*c(5,5)==756") {
    val result = group(List(2,2,5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
    println(result)
    assert(756 === result.size)
  }