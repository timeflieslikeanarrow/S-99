package com.s99.arithmetic

class S99Int(val start: Int):
  import S99Int._

  def isPrime: Boolean = (2 to start - 1).forall( start % _ != 0)

  //Two numbers are coprime if their greatest common divisor equals 1.
  def isCoprimeTo(n: Int): Boolean = gcd(start, n) == 1
  
  //Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are coprime to m.
  def totient: Int = (1 to start).count(isCoprimeTo(_))
  
  def primeFactors: List[Int] =
    def iter(n: Int, factor: Int, acc: List[Int]): List[Int] = 
      if n == 1 then acc.reverse
      else if n % factor == 0 then 
        iter(n/factor, factor, factor::acc)
      else iter(n, factor + 1, acc)
//    val result = scala.collection.mutable.ListBuffer[Int]()
//    var n = start
//    var i = 2
//    while n > 1 do
//      if n % i == 0 then
//        result += i
//        n /= i
//      else
//        i += 1
//  
//    result.toList
    iter(start, 2, List())
  end primeFactors

  def primeFactorMultiplicity: Map[Int, Int] =
    val result = collection.mutable.Map[Int, Int]().withDefaultValue(0)
    primeFactors.foreach(n => result(n) += 1)
    result.toMap

  def totientImproved: Int =
    primeFactorMultiplicity.foldLeft(1) { case (result, (k,v)) =>
      result * (k - 1) * math.pow(k, v - 1).toInt
    }
  
  //Goldbach's conjecture
  //Goldbach's conjecture says that every positive even number greater than 2 
  // is the sum of two prime numbers. E.g. 28 = 5 + 23. 
  // It is one of the most famous facts in number theory that has not been proved 
  // to be correct in the general case. It has been numerically confirmed up to very large numbers 
  // (much larger than Scala's Int can represent). Write a function to find the two prime numbers 
  // that sum up to a given even integer.
  def goldbach: (Int,Int) =
    def iter(n: Int):(Int, Int) =
      if n.isPrime && (start - n).isPrime then (n, start - n)
      else iter(n+1)
    if start <= 2 || start % 2 != 0 then throw IllegalArgumentException("must be an even number")
    else iter(2)
end S99Int

object S99Int:
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  
  //Use Euclid's algorithm.
  def gcd(p: Int, q: Int): Int =
    if q == 0 then p else gcd(q, p % q)

  //Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
  def listPrimesInRange(r: Range): List[Int] =
    r.filter(_.isPrime).toList

  def printGoldbachList(r: Range): List[(Int, (Int, Int))] =
    r.filter (n => n > 2 && n % 2 == 0).map { n => (n, n.goldbach)}.toList

  def printGoldbachListLimited(r: Range, minFactor: Int): List[(Int, (Int, Int))] =
    printGoldbachList(r).filter { case (_, (s, l)) => s > minFactor && l > minFactor}

end S99Int

