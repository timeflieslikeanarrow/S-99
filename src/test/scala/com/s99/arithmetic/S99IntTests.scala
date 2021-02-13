package com.s99.arithmetic

import S99Int._

import org.scalatest.funsuite.AnyFunSuite

class S99IntTests extends AnyFunSuite:
  test("P31 - Determine whether a given integer number is prime") {
    assert(7.isPrime)
    assert(!15.isPrime)
  }

  test("P32 - Determine the greatest common divisor of two positive integer numbers.") {
    assert(9 === gcd(36,63))
  }

  test("P33 - Determine whether two positive integer numbers are coprime.") {
    assert(35.isCoprimeTo(64))
  }

  test("P34 - Calculate Euler's totient function phi(m).") {
    assert(4 == 10.totient)
  }

  test("P35 - Determine the prime factors of a given positive integer.") {
    assert(List(3,3,5,7) === 315.primeFactors)
  }

  test("P36 - Determine the prime factors of a given positive integer (2") {
    assert(Map(3 -> 2, 5 -> 1, 7 -> 1) == 315.primeFactorMultiplicity)
  }

  test("P37 -- Calculate Euler's totient function phi(m) (improved)") {
    assert((2-1) * math.pow(2,1-1) * (5-1) * math.pow(5, 1-1)  === 10.totientImproved)
    assert((3-1) * math.pow(3, 2-1) * (5-1) * math.pow(5, 1-1) * (7-1) * math.pow(7, 1-1) === 315.totientImproved)
  }
  
  def time[T](block: => T): (T, Long) =
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    (result, end - start)
  
  test("P38 - Compare the two methods of calculating Euler's totient function") {
    val number = 10090
    val (result1, timeElapsed1) = time {
      number.totient
    }

    val (result2, timeElapsed2) = time {
      number.totientImproved
    }
    
    assert(result1 === result2)
    println(s"totient for $number used: $timeElapsed1 ns - totientImproved used: $timeElapsed2 ns")
    assert(timeElapsed1 > timeElapsed2)
  }

  test("P39 -- A list of prime numbers") {
    assert(List(7, 11, 13, 17, 19, 23, 29, 31) === listPrimesInRange(7 to 31))
  }

  test("P40 -- Goldbach's conjecture") {
    assertThrows[IllegalArgumentException] {
      2.goldbach
    }
    
    assertThrows[IllegalArgumentException] {
      11.goldbach
    }
    
    assert((5, 23) == 28.goldbach)
  }
  
  test("P41 -- A list of Goldbach compositions") {
    info("printGoldbachList")
    assert(List((10, (3,7)),
      (12, (5,7)),
      (14, (3, 11)),
      (16, (3, 13)),
      (18, (5, 13)),
      (20, (3, 17))
    ) === printGoldbachList(9 to 20))
    
    info("printGoldbachListLimited")
    assert(List((992, (73, 919)),
      (1382, (61, 1321)),
      (1856, (67, 1789)),
      (1928, (61, 1867))
    ) === printGoldbachListLimited(1 to 2000, 50))
  }