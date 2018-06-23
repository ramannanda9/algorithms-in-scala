package com.blogspot.ramannanda.scala.fpinscala.iomonads.free


import com.blogspot.ramannanda.scala.fpinscala.applicative.Monad
import com.blogspot.ramannanda.scala.fpinscala.iomonads.free.Free.{translate, ~>}
import com.blogspot.ramannanda.scala.fpinscala.parallelism
import com.blogspot.ramannanda.scala.fpinscala.parallelism.Par

import scala.annotation.tailrec
import scala.io.StdIn
import scala.language.{higherKinds, reflectiveCalls}

sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = {
    FlatMap(this, f)
  }

  def map[B](f: A => B): Free[F, B] = {
    flatMap(f andThen (Return(_)))
  }

}

case class Return[F[_], A](a: A) extends Free[F, A]

case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

sealed trait Console[A] {
  def toPar: Par[A]

  def toThunk: () => A
}

case object ReadLine extends Console[Option[String]] {
  override def toThunk: () => Option[String] = () => run

  override def toPar = Par.lazyUnit(run)

  def run: Option[String] = {
    try {
      Some(StdIn.readLine())
    }
    catch {
      case _: Exception => None
    }
  }
}

case class PrintLine(line: String) extends Console[Unit] {
  override def toPar: Par[Unit] = Par.lazyUnit(println(line))

  override def toThunk: () => Unit = () => println(line)
}


object Console {
  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

  val consoleToFunction0 = new (Console ~> Function0) {
    override def apply[A](F: Console[A]): () => A = F.toThunk
  }
  val consoleToPar = new (Console ~> Par) {
    override def apply[A](F: Console[A]): Par[A] = F.toPar
  }

  implicit val function0Monad = new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a

    override def flatMap[A, B](fa: () => A)(f: A => () => B): () => B = {
      f(fa())
    }
  }

  implicit val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = Par.fork {
      Par.flatMap(fa)(f)
    }
  }

  def runConsoleFunction0[A](free: Free[Console, A]): () => A = {
    Free.runFree(free)(consoleToFunction0)
  }

  def runConsolePar[A](free: Free[Console, A]): Par[A] = {
    Free.runFree(free)(consoleToPar)
  }

  def runConsole[A](a: Free[Console, A]): A =
    Free.runTrampoline {
      translate(a)(consoleToFunction0)
    }


}


trait Translate[F[_], G[_]] {
  def apply[A](F: F[A]): G[A]
}

trait Source {
  def readBytes(numBytes: Int, callback: Either[Throwable, Array[Byte]] => Unit): Unit
}

object Free {
  type ~>[F[_], G[_]] = Translate[F, G]

  //wraps the translate into Suspend to get F->Free[G,A]
  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[A] = Free[G, A]
    val t = new (F ~> FreeG) {
      override def apply[A](a: F[A]): FreeG[A] = Suspend(fg(a))
    }
    runFree(f)(t)(freeMonad[G])
  }


  def freeMonad[F[_]]: Monad[({type f[x] = Free[F, x]})#f] = {
    new Monad[({type f[x] = Free[F, x]})#f] {
      override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = {
        fa flatMap f
      }

      override def unit[A](a: => A): Free[F, A] = Return(a)
    }
  }

  @tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = {
    a match {
      case Return(a1) => a1
      case Suspend(s) => s()
      case FlatMap(x, f) => x match {
        case FlatMap(y, g) => runTrampoline(y flatMap (a1 => g(a1) flatMap (f)))
        case Suspend(s) => runTrampoline(f(s()))
        case Return(a1) => runTrampoline(f(a1))
      }
    }
  }

  def step[F[_], A](a: Free[F, A]): Free[F, A] = {
    a match {
      case FlatMap(Return(a1), f) => step(f(a1))
      case FlatMap(FlatMap(y, g), f) => step(y flatMap (a1 => g(a1) flatMap (f)))
      case _ => a
    }
  }

  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = {
    step(a) match {
      case Return(a1) => F.unit(a1)
      case Suspend(r) => r
      case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Not believable this")
    }
  }

  /**
    * we use translate to translate the type for which there is no way to write flatmap to
    * a type for which Monad is available.
    */
  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] = {
    step(free) match {
      case Return(a1) => G.unit(a1)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(g => runFree(f(g))(t))
      case _ => sys.error("Unbelievable, just how?")
    }
  }
}
