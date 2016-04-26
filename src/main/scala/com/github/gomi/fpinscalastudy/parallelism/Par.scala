package com.github.gomi.fpinscalastudy.parallelism

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

trait Par[+A]

object Par {

  type Par[A] = ExecutorService => Future[A]

  // 直ちに a 値が得られる計算を作成
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  // 式 a を run による並列評価のためにマッピング
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // 与えられた Par を完全に評価し、fork によって要求される並列計算を生成し、結果の値を取得
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // run による並列評価の対象としてマーク
  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call(): A = a(es).get
  })

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((a, _) => a.sorted)

//  def sum(ints: IndexedSeq[Int]): Par[Int] =
//    if (ints.size <= 1) {
//      Par.unit(ints.headOption.getOrElse(0))
//    } else {
//      val (l, r) = ints.splitAt(ints.length / 2)
//      Par.map2(sum(l), sum(r))(_ + _)
//    }

  // 2つの並列計算の結果を2項関数で結合
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone: Boolean = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

}
