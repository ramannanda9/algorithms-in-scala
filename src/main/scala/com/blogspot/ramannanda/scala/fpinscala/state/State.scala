package com.blogspot.ramannanda.scala.fpinscala.state


trait RNG {
  def nextInt: (Int, RNG)
}

import State._

case class State[S, +A](run: S => (A, S)) {


  def map[B](f: A => B): State[S, B] = {
    State(s => {
      val (a, s1) = run(s)
      (f(a), s1)
    })
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State(s => {
      val (a, s1) = run(s)
      val (b, s2) = sb.run(s1)
      (f(a, b), s2)
    })
  }

  def flatMap[B](g: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, s1) = run(s)
      g(a).run(s1)
    })
  }

  def map2UsingFl[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.flatMap(b => unit(f(a, b))))
  }

  def mapUsingFL[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a)))
  }


}


object State {
  def unit[S, A](a: A): State[S, A] = {
    State(s => (a, s))
  }

  def sequence[S, A](sb: List[State[S, A]]): State[S, List[A]] = {
    sb.foldRight(unit[S, List[A]](List()))((s, a) => s.map2(a)(_ :: _))
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i1, r1) = nonNegativeInt(rng)
    val (i2, r2) = double(r1)
    ((i1, i2), r2)
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match {
      case (i, rng2) => (i % 2 == 0, rng2)
    }

  def unit[A](a: A): Rand[A] = {
    r => (a, r)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)(i => i - i % 2)
  }

  def doubleUsingMap: Rand[Double] = {
    map(nonNegativeInt)(i => i / (Int.MaxValue + 1))
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs match {
      case h :: t => map2(h, sequence(t))(_ :: _)
      case _ => unit(Nil)
    }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + n - 1 - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    })
  }

  def map2UsingFl[A, B, C](randA: Rand[A], randB: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(randA)(a => flatMap(randB)(b => unit(f(a, b))))
  }

}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyDispenser {
  def update = (i: Input) => (s: Machine) => (i, s) match {
    case (_, Machine(_, 0, _)) => s
    case (Coin, Machine(false, _, _)) => s
    case (Turn, Machine(true, _, _)) => s
    case (Coin, Machine(true, candy, coins)) => Machine(locked = false, candy, coins + 1)
    case (Turn, Machine(false, candies, coins)) => Machine(locked = true, candies - 1, coins)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      _ <- sequence(inputs.map(modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)
  }
}
