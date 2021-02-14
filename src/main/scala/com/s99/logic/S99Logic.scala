package com.s99.logic

object S99Logic:
  def not(a: Boolean): Boolean = if a then false else true
  def and(a: Boolean, b: => Boolean): Boolean = if a then b else false
  def or(a: Boolean, b: => Boolean): Boolean  = if a then a else b
  
  def nand(a: Boolean, b: => Boolean): Boolean  = not (and(a, b))
  def nor(a: Boolean, b: => Boolean): Boolean  = not (or(a,b))
  
  def xor(a: Boolean, b: Boolean): Boolean  = not (a == b)

  def impl(a: Boolean, b: =>Boolean): Boolean = if a then b else true
  def equ(a: Boolean, b: Boolean): Boolean = a == b

  def table2(f: (a:Boolean, b: Boolean) => Boolean): String = 
    def compute(): List[List[Boolean]] = 
      val truths = List(true, false)
      for { 
        a <- truths
        b <- truths
      } yield List(a, b, f(a,b) )
    
    (List("A", "\tB", "\tresult") :: compute()).map(_.mkString("\t")).mkString("\n")
    
  def gray(n: Int): List[String] =
    def iter(i: Int, accs: List[String]): List[String] =
      if i == n then accs
      else
        val data = List("0", "1")
        val result =  if accs.isEmpty then data
                      else
                          for {
                          digit <- data
                          acc <- if digit == "0" then accs else accs.reverse
                        } yield digit + acc
        iter(i+1, result)
          
    iter(0, List())
  
  //a simple implementation of Huffman Coding
  def huffman(frequencies: List[(String, Int)]): List[(String, String)] =
    val children = scala.collection.mutable.Map[String, List[String]]()
    val pq = scala.collection.mutable.PriorityQueue.empty[(Int, String)].reverse
    
    def collect() =
      val revereseFrequencies = frequencies.map(_.swap)
      revereseFrequencies.foreach(pq.enqueue(_))
      while pq.size >= 2 do
        val (w1, item1) = pq.dequeue()
        val (w2, item2) = pq.dequeue()
        pq.enqueue((w1+w2, item1+item2))
        children(item1 + item2) = List(item1, item2)

    val result = scala.collection.mutable.ListBuffer[(String, String)]()
    def coding(parent: String, code: String): Unit =
      if children.contains(parent) then
        val childList = children.get(parent).get
        val left = childList(0)
        val right = childList(1)
        coding(left, code + "0")
        coding(right, code + "1")
      else
        result += parent -> code
        
    collect()
    println(pq)
    println(children)
    val (w, root) = pq.dequeue()
    coding (root, "")
    
    result.toList

class Bool(truth: Boolean):
  import Bool._
  
  def and(other: => Boolean): Boolean = if truth then other else false
  def or(other: => Boolean): Boolean = if truth then truth else other
  def nand(other: => Boolean): Boolean =  not(and(other))
  def nor(other: => Boolean):Boolean = not(or(other))
  def xor(other: Boolean):Boolean = truth != other 
  def impl(other: => Boolean):Boolean = if truth then other else true
  def equ(other: Boolean):Boolean = truth == other

  def table2(f: (a:Boolean, b: Boolean) => Boolean): String =
    def compute(): List[List[Boolean]] =
      val truths = List(true, false)
      for {
        a <- truths
        b <- truths
      } yield List(a, b, f(a,b) )

    (List("A", "\tB", "\tresult") :: compute()).map(_.mkString("\t")).mkString("\n")
  
object Bool:
  implicit def convert(a: Boolean): Bool = new Bool(a)
  def not(a: Boolean): Boolean = if a then false else true