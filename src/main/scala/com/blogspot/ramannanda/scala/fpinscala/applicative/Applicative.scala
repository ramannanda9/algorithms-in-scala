package com.blogspot.ramannanda.scala.fpinscala.applicative


import com.blogspot.ramannanda.scala.fpinscala.monads.Functor
import com.blogspot.ramannanda.scala.fpinscala.monoids.{Foldable, Monoid}
import com.blogspot.ramannanda.scala.fpinscala.state.State
import com.blogspot.ramannanda.scala.fpinscala.state.State._

import language.reflectiveCalls
import language.higherKinds
import language.reflectiveCalls
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2(fab, fa)((f, a) => f(a))
  }

  def unit[A](a: => A): F[A]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
    apply(unit(f))(fa)
  }

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    apply(apply(unit(f.curried))(fa))(fb)
  }

  def map2WithMap[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    apply(map(fa)(f.curried))(fb)
  }

  def applyWithMap2[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2(fab, fa)((f, a) => f(a))
  }

  def sequence[A](fas: List[F[A]]): F[List[A]] = {
    fas.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    ofa.foldLeft(unit(Map[K, V]()))((acc, kv) =>
      map2(acc, kv._2)((m, v) => m + (kv._1 -> v)))
  }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
    sequence(List.fill(n)(fa))
  }

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
    map2(fa, fb)((_, _))
  }

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) = {
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
      }
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] = {
        self.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))
      }
    }
  }


  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
    la.foldRight(unit(List[B]()))((a, acc) => map2(f(a), acc)(_ :: _))
  }

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
  }

}


sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {

  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
      (fa, fb) match {
        case (Success(_), u@Failure(_, _)) => u
        case (u@Failure(_, _), Success(_)) => u
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, (t1 :+ h2) ++ t2)
        case (Success(a1), Success(a2)) => Success(f(a1, a2))
      }
    }

    override def unit[A](a: => A): Validation[E, A] = {
      Success(a)
    }
  }

  type Const[M, B] = M

  implicit def monoidApplicative[M](m: Monoid[M]) = {
    new Applicative[({type f[x] = Const[M, x]})#f] {
      override def unit[A](a: => A): Const[M, A] = m.zero

      override def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(f: (A, B) => C): Const[M, C] = {
        m.op(fa, fb)
      }
    }
  }
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  //left abstract minimal set
  def unit[A](a: => A): F[A]

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    flatMap(fa)(a => map(fb)(f(a, _)))
  }

  //Kleisli compose
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a => flatMap(f(a))(g)
  }

  def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Monad[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      //Not possible as distributive law is not applicable to Monads
      override def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] = ???
    }
  }

  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
    flatMap(fa)(a => unit(f(a)))
  }

  def join[A](ffa: F[F[A]]): F[A] = {
    flatMap(ffa)(a => a)
  }

}


object Monad {
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = {
      fa match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
      }
    }

    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def apply[A, B](fab: Either[E, A => B])(fa: Either[E, A]): Either[E, B] = {
      fa match {
        case Left(e) => Left(e)
        case Right(a) => fab.right.map(v => v(a))
      }
    }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = {
      State(s => {
        val (a, s1) = fa.run(s)
        f(a).run(s1)
      })
    }
  }

  def composeM[G[_], H[_]](implicit G: Monad[G], H: Monad[H], T: Traverse[H]): Monad[({type f[x] = G[H[x]]})#f] = {
    new Monad[({type f[x] = G[H[x]]})#f] {
      override def flatMap[A, B](fa: G[H[A]])(f: A => G[H[B]]): G[H[B]] = {
        G.flatMap(fa)(ha => G.map(T.traverse(ha)(f))(H.join))
      }

      override def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))
    }
  }


}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>

  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_], A](fma: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a

    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
    traverse[Id, A, B](fa)(f)(idMonad)
  }

  import Applicative._

  //here G becomes B and B nothing F[Nothing] is Nothing
  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = {
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(mb)
  }


  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] = {
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)
  }

  def zipWithIndex[A](t: F[A]): F[(A, Int)] = {
    traverseS(t)(a => for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a, i)).run(0)._1
  }

  override def toList[A](fa: F[A]): List[A] = {
    traverseS(fa)((a: A) => for {
      as <- get[List[A]]
      _ <- set(a :: as)
    } yield ()).run(Nil)._2.reverse
  }

  //s is like z or initial state
  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = {
    traverseS(fa)(a => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)
  }

  def reverse[A](fa: F[A]): F[A] = {
    mapAccum(fa, toList(fa).reverse)((_, s) => (s.head, s.tail))._1
  }

  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
    mapAccum(as, z)((a, s) => ((), f(s, a)))._2
  }

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] = {
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    }._1
  }

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] = {
    mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    }._1
  }

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

  }

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_], A, B](fga: F[G[A]])(f: A => M[B])(implicit M: Applicative[M]) = {
        self.traverse(fga)(ga => G.traverse(ga)(f))
      }
    }
  }


}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {

  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = {
      fa.foldRight(G.unit(List[B]()))((a, acc) => G.map2(f(a), acc)(_ :: _))
    }
  }
  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = {
      fa match {
        case None => G.unit(None)
        case Some(v) => G.map(f(v))(Some(_))
      }
    }
  }
  val TreeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = {
      G.map2(f(fa.head), listTraverse.traverse(fa.tail)(a => traverse(a)(f)))((h, t) => Tree(h, t))
    }
  }


}



