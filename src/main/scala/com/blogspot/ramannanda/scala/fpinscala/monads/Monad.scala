package com.blogspot.ramannanda.scala.fpinscala.monads

import scala.language.higherKinds
import scala.language.reflectiveCalls

trait Functor[F[_]] {
  def map[A, B](a: F[A])(f: A => B): F[B]

  //like unzip
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = {
    (map(fab)(_._1), map(fab)(_._2))
  }

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = {
    e match {
      case Left(l) => map(l)(Left(_))
      case Right(r) => map(r)(Right(_))
    }
  }
}

trait Monad[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    flatMap(fa)(a => unit(f(a)))
  }

  def _flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = {
    compose((_: Unit) => fa, f)(())
  }

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  //left abstract minimal set
  def unit[A](a: => A): F[A]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    flatMap(fa)(a => map(fb)(f(a, _)))
  }

  def sequenceUnidiomatic[A](lma: List[F[A]]): F[List[A]] = {
    lma match {
      case h :: t => map2(h, sequenceUnidiomatic(t))(_ :: _)
      case _ => unit(Nil)
    }
  }

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma.foldRight(unit(List[A]()))((a: F[A], acc: F[List[A]]) => map2(a, acc)(_ :: _))
  }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
    la.foldRight(unit(List[B]()))((a, acc) => map2(f(a), acc)(_ :: _))
  }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
    sequence(List.fill(n)(fa))
  }

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  //Kleisli compose
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a => flatMap(f(a))(g)
  }

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ms.foldRight(unit(List[A]()))((a, acc) =>
      compose(f, (b: Boolean) => if (b) map2(unit(a), acc)(_ :: _) else acc)(a))
  }

  def join[A](ffa: F[F[A]]): F[A] = {
    flatMap(ffa)(a => a)
  }

  def composeInTermsOfJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a => join(map(f(a))(g))
  }

  def flatMapInTermsOfJoin[A, B](fa: F[A])(f: A => F[B]): F[B] = {
    join(map(fa)(f))
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = {
    f(value)
  }
}

object Id {
  val monadId: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = {
      fa.flatMap(f)
    }
  }
}


object Monad {

  case class State[S, A](run: S => (A, S))

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = {
      State(s => {
        val (a, s1) = fa.run(s)
         f(a).run(s1)
      })
    }
  }

}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = {
      Reader(r => {
        val a = st.run(r)
         f(a).run(r)
      })
    }
  }
}
