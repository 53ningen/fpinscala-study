package com.github.gomi.fpinscalastudy.lazyness

import Stream._

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


sealed trait Stream[+A] {

  def isEmpty: Boolean = this match {
    case Cons(_, _) => true
    case _ => false
  }

  def tailOption: Option[Stream[A]] = this match {
    case Empty => None
    case Cons(_, t) => Some(t())
  }

  def toList: List[A] = {
    @scala.annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }


  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, acc) => p(a) && acc)

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else acc)

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else acc)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((b, bs) => cons(b, bs))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, acc) => f(a).append(acc))

  def foldLeft[B](z: => B)(f: (=> B, A) => B): B = {
    var acc = z
    var these = this
    while (!these.isEmpty) {
      acc = f(acc, these.headOption.get)
      these = these.tailOption.get
    }
    acc
  }

  def _map[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def _take(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), x) if x > 0 => Some(h(), (t(), x - 1))
      case _ => None
    }

  def _takeWhile(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()), (ta(), tb()))
      case _ => None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this, s) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (empty, t()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), empty))
      case _ => None
    }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), a) => if (a > 0) Some(h(), (t(), a - 1)) else None
      case _ => None
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined).forAll {
      case (Some(a), Some(b)) => a == b
      case _ => false
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) => if (p(h())) Some(h(), t()) else None
      case _ => None
    }


  def zipWithViaUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()), (ta(), tb()))
      case _ => None
    }

  def zipAllViaUnfold[B](s2: Stream[B]): Stream[(A, B)] =
    zipWithViaUnfold(s2)((_, _))

  def _zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
      case (Cons(ha, ta), _) => Some((Some(ha()), None), (ta(), empty))
      case (_, Cons(hb, tb)) => Some((None, Some(hb())), (empty, tb()))
      case _ => None
    }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some(s, s.drop(1))
    }.append(empty)


  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })
      ._2

}

object Stream {

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons[A](a, unfold(s)(f))
    case None => empty
  }

  def ones2: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def fibs2: Stream[Int] = unfold((0, 1)) { case (n1, n2) => Some(n1, (n1, n1 + n2)) }

  def _fibs: Stream[Int] = unfold((0, 1)) { case (n1, n2) => Some(n1, (n1, n1 + n2)) }

  def _ones: Stream[Int] = unfold(1) { _ => Some(1, 1) }

  def _constant[A](a: A): Stream[A] = unfold(a) { _ => Some(a, a) }

  def _from(n: Int): Stream[Int] = unfold(n)(a => Some(a, a + 1))

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}
