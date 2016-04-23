package com.github.gomi.fpinscalastudy.datastructures

import org.scalatest.{Matchers, FunSuite}

class ListTest extends FunSuite with Matchers {

  test("List") {
    val list = List(1, 2, 3, 4, 5)
    list.isEmpty should be(false)
    list.reverse should be(List(5, 4, 3, 2, 1))
  }

}
