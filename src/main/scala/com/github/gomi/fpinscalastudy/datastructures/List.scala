package com.github.gomi.fpinscalastudy.datastructures

import scala.annotation.tailrec

sealed trait List[+A] {

  def isEmpty: Boolean = this match {
    case Nil => true
    case _ => false
  }

  def fmap[B](f: A => B): List[B] = List.fmap(this)(f)

  def reverse: List[A] = List.reverse(this)

  def filter(f: A => Boolean): List[A] = List.filter(this)(f)

}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](values: A*): List[A] =
    if (values.isEmpty) Nil
    else Cons(values.head, apply(values.tail: _*))

  def fmap[A, B](l: List[A])(f: A => B): List[B] = {
    @tailrec
    def go(l: List[A], buf: List[B]): List[B] = l match {
      case Nil => buf
      case Cons(h, t) => go(t, Cons(f(h), buf))
    }
    def hoge(): Unit = {}
    go(l, Nil).reverse
  }

  def reverse[A](l: List[A]): List[A] = {
    @tailrec
    def go(l: List[A], buf: List[A]): List[A] = l match {
      case Nil => buf
      case Cons(h, t) => go(t, Cons(h, buf))
    }
    go(l, Nil)
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    @tailrec
    def go(l: List[A], buf: List[A]): List[A] = l match {
      case Nil => buf
      case Cons(h, t) =>
        if (f(h)) go(t, Cons(h, buf))
        else go(t, buf)
    }
    go(l, Nil).reverse
  }

  def length[A](list: List[A]): Int = {
    @tailrec
    def go(l: List[A], num: Int): Int = l match {
      case Nil => num
      case cons: Cons[A] => go(cons.tail, num + 1)
    }
    go(list, 0)
  }

}
