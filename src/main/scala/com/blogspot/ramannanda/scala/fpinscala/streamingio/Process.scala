package com.blogspot.ramannanda.scala.fpinscala.streamingio

sealed trait Process[I, O] {
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process[I, O] = {
    def loop(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => loop(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i => loop(recv(i))
      }
      case Emit(h, t) => Emit(h, loop(t))
    }

    loop(this)
  }

}

case class Emit[I, O](h: O, t: Process[I, O] = Halt[I, O]()) extends Process[I, O]

case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]

case class Halt[I, O]() extends Process[I, O]

object Process {

  def liftOne[I, O](f: I => O): Process[I, O] = {
    Await {
      case Some(i) => emit(f(i))
      case None => Halt()
    }
  }

  def emit[I, O](head: O,
                 tail: Process[I, O] = Halt[I, O]()): Process[I, O] =
    Emit(head, tail)

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(i) if p(i) => emit(i)
      case _ => Halt()
    }.repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] =
      Await {
        case Some(d) => Emit(d + acc, go(d + acc))
        case None => Halt()
      }

    go(0.0)
  }

  def take[I](n: Int): Process[I, I] = {
    if (n <= 0) Halt()
    else Await {
      case Some(d) => Emit(d, take(n - 1))
    }
  }

  def drop[I](n: Int): Process[I, I] = {
    if (n > 0) Await {
      i => drop(n - 1)
    }
    else id
  }

  def id[I]: Process[I, I] = lift(identity)


  def takeWhile[I](f: I => Boolean): Process[I, I] = {
    Await {
      case Some(d) if f(d) => emit(d, takeWhile(f))
      case _ => Halt()
    }
  }

  def dropWhile[I](f: I => Boolean): Process[I, I] = {
    await(i =>
      if (f(i)) dropWhile(f)
      else emit(i, id[I])
    )

  }

  def count[I]: Process[I, Int] = {
    def countInt(acc: Int = 1): Process[I, Int] = {
      await(i => emit(acc, countInt(acc + 1)))
    }

    countInt()
  }

  def mean: Process[Double, Double] = {
    def mean(sum: Double, count: Double): Process[Double, Double] = {
      await(i => {
        val avg = (sum + i) / (count + 1)
        emit(avg, mean(sum + i, count + 1))
      })
    }

    mean(0.0,0)
  }

  def await[I, O](f: I => Process[I, O],
                  fallback: Process[I, O] = Halt[I, O]()): Process[I, O] =
    Await[I, O] {
      case Some(i) => f(i)
      case None => fallback
    }


}