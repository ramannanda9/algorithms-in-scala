package com.blogspot.ramannanda.scala.fpinscala.iomonads.io



import scala.annotation.tailrec

sealed trait IO[A] {
  def map[B](f: A => B): IO[B] = {
    flatMap(f andThen (Return(_)))
  }

  def flatMap[B](f: A => IO[B]): IO[B] = {
    FlatMap(this, f)
  }

}

case class Return[A](a: A) extends IO[A]

case class Suspend[A](resume: () => A) extends IO[A]

case class FlatMap[A, B](sub: IO[A], g: A => IO[B]) extends IO[B]


object IO {
  @tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(s, f) => s match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }

}