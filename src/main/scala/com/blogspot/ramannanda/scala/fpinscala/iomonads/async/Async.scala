package com.blogspot.ramannanda.scala.fpinscala.iomonads.async

import com.blogspot.ramannanda.scala.fpinscala.applicative.Monad
import com.blogspot.ramannanda.scala.fpinscala.parallelism.Par
import com.blogspot.ramannanda.scala.fpinscala.parallelism.Par.Par

import scala.annotation.tailrec

object Asyncs {

  sealed trait Async[A] {
    def map[B](f: A => B): Async[B] = {
      flatMap(f andThen (Return(_)))
    }

    def flatMap[B](f: A => Async[B]): Async[B] = {
      FlatMap(this, f)
    }
  }

  case class Return[A](a: A) extends Async[A]

  case class Suspend[A](resume: Par[A]) extends Async[A]

  case class FlatMap[A, B](sub: Async[A],
                           k: A => Async[B]) extends Async[B]

  object Async extends Monad[Async] {
    override def unit[A](a: => A): Async[A] = Return(a)

    override def flatMap[A, B](fa: Async[A])(f: A => Async[B]): Async[B] = fa flatMap f
  }

  @tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x, g), f) => step(x flatMap (a => g(a) flatMap f))
    case FlatMap(Return(a), f) => step(f(a))
    case _ => async
  }

  def run[A](async: Async[A]): Par[A] = {
    step(async) match {
      case Return(a) => Par.unit(a)
      case Suspend(r) => r
      case FlatMap(x, f) => x match {
        case Suspend(r1) => Par.flatMap(r1)(a => run(f(a)))
        case _ => sys.error("No other cases are possible")
      }
    }
  }

}
