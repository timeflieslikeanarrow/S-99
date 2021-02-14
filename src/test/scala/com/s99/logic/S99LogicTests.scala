package com.s99.logic

import org.scalatest.funsuite.AnyFunSuite

import S99Logic._

class S99LogicTests extends AnyFunSuite:
  test("P46 - Truth tables for logical expressions") {
    assert(true === and(true, true))
    assert(false === and(false, true))
    
    assert(false === nand(true, true))
    assert(true === nand(false, true))
    
    assert(true === or(true, false))
    assert(false === or (false, false))

    assert(false === nor(true, false))
    assert(true === nor (false, false))
    
    assert(false === xor(true, true))
    
    assert(true === impl(true, true))
    assert(true === impl(false, true))
    
    println(equ(true, true))
    assert(true === equ(true, true))
    assert(true === equ(false, false))
    assert(false === equ(true, false))
    assert(false === equ(false, true))
    
    println(table2((a, b) => and (a, or(a, b))))
  }

  test("P47 - Truth tables for logical expressions (2)") {
    import Bool._
    assert(true === true and true)
    assert(false === false and true)
    
    assert(false === true nand true)
    assert(true === false nand true)

    assert(true === true or false)
    assert(false === false or false)

    assert(false === true nor false)
    assert(true === false nor false)

    assert(false === true xor true)

    assert(true === true impl true)
    assert(true === false impl true)
    
    assert(true === true equ true)
    assert(true === false equ false)
    assert(false === true equ false)
    assert(false === false equ true)

    println(table2((a, b) => a and (a or not(b))))
  }

  test("P49 - Gray code") {
    assert(List("0", "1") === gray(1))
    assert(List("00", "01", "11", "10") === gray(2))
    assert(List("000", "001", "011", "010", "110", "111", "101", "100") === gray(3))
  }
