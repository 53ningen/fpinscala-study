package com.github.gomi.fpinscalastudy.state

trait RNG {

  type Rand[+A] = RNG => (A, RNG)

  val nextInt: (Int, RNG)

  val int: Rand[Int] = _.nextInt

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i == Int.MinValue) nonNegativeInt(rng2)
    else (i.abs, rng2)
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def _map[A, B](s: RNG => (A, RNG))(f: A => B): RNG => (B, RNG) = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = { rng =>
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  val nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  val double: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod > 0) (mod, rng2)
    else nonNegativeLessThan(n)(rng2)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def _flatMap[A, B]
  (f: RNG => (A, RNG))
  (g: A => (RNG => (B, RNG))): RNG => (B, RNG) =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def __map[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def ___map2[A, B, C](r1: Rand[A], r2: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(r1) { a =>
      map(r2) { b =>
        f(a, b)
      }
    }

  def _nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod > 0) unit(mod)
    else _nonNegativeLessThan(n)
  }

}

object RNG {

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      val (_as, _rng) = fs.foldLeft((Nil: List[A], rng)) { case ((as, rng2), rand) =>
        val (a, rng3) = rand(rng2)
        (a :: as, rng3)
      }
      (_as.reverse, _rng)
    }


  type Rand[+A] = RNG => (A, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i == Int.MinValue) nonNegativeInt(rng2)
    else (i.abs, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (Int.MaxValue / (Int.MaxValue.toDouble + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (v, rng2) = intDouble(rng)
    (v.swap, rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @scala.annotation.tailrec
    def go(count: Int, state: (List[Int], RNG)): (List[Int], RNG) = {
      if (count < 1) {
        state
      } else {
        val (i, r2) = state._2.nextInt
        go(count - 1, (i :: state._1, r2))
      }
    }
    go(count, (List[Int](), rng))
  }

}

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s2) = run(s)
    (f(a), s2)
  })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (a, s2) = run(s)
    val (b, s3) = sb.run(s2)
    (f(a, b), s3)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })

}

object State {

  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: Seq[State[S, A]]): State[S, Seq[A]] =
    sas.foldRight(unit[S, Seq[A]](Seq[A]())) { (stateA, acc) =>
      stateA.map2(acc)((a, as) => a +: as)
    }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) => Machine(locked = false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(locked = true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs map (State.modify[Machine] _ compose update))
    s <- State.get
  } yield (s.coins, s.candies)

}
