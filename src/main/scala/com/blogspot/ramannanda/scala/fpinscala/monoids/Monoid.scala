package com.blogspot.ramannanda.scala.fpinscala.monoids

import scala.annotation.tailrec

trait Monoid[A] {
  def zero: A

  def op(a: A, b: A): A
}

object Monoid {
  def intMonoid = new Monoid[Int] {
    override def zero: Int = 0

    override def op(a: Int, b: Int): Int = {
      a + b
    }
  }

  def intMultiplication = new Monoid[Int] {
    override def zero: Int = 1

    override def op(a: Int, b: Int): Int = a * b
  }

  def booleanOr = new Monoid[Boolean] {
    override def zero: Boolean = false

    override def op(a: Boolean, b: Boolean): Boolean = a | b
  }

  def booleanAnd = new Monoid[Boolean] {
    override def zero: Boolean = true

    override def op(a: Boolean, b: Boolean): Boolean = a && b
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    override def zero: Option[A] = None

    override def op(a: Option[A], b: Option[A]): Option[A] = a orElse (b)
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def zero: A => A = (a: A) => a

    override def op(a: A => A, b: A => A): A => A = a.compose(b)
  }

  def dual[A](monoid: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def zero: A = monoid.zero

    override def op(a: A, b: A): A = monoid.op(b, a)
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((acc: B, a: A) => m.op(acc, f(a)))
  }

  def foldLeftFm[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)
  }

  def foldRightFm[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty) m.zero
    val splitAt = as.size / 2
    if (splitAt == 0) return f(as.head)
    val (leftAs, rightAs) = as.splitAt(splitAt)
    m.op(foldMapV(leftAs, m)(f), foldMapV(rightAs, m)(f))
  }

  def isOrdered[A](as: IndexedSeq[A])(implicit ev: Ordering[A]): Boolean = {
    import ev.mkOrderingOps
    val mon = new Monoid[Option[(A, A, Boolean)]] {
      val zero = None

      override def op(a: Option[(A, A, Boolean)], b: Option[(A, A, Boolean)]): Option[(A, A, Boolean)] = {
        (a, b) match {
          case (Some((x1, y1, b1)), Some((x2, y2, b2))) => Some((x1 min x2, y1 max y2, b1 && b2
            && (y1 <= x2)))
          case (x, None) => x
          case (None, x) => x
        }
      }
    }
    foldMapV(as, mon)(v => Some(v, v, true)).map(_._3).getOrElse(true)
  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC


  val wcm: Monoid[WC] = new Monoid[WC] {
    override def zero: WC = Stub("")

    override def op(a: WC, b: WC): WC = {
      (a, b) match {
        case (Stub(t), Stub(t1)) => Stub(t + t1)
        case (Stub(c), Part(l, i, r)) => Part(c + l, i, r)
        case (Part(l, i, r), Stub(c)) => Part(c, i, r + c)
        case (Part(l1, i1, r1), Part(l2, i2, r2)) =>
          Part(l1, i1 + i2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
      }
    }
  }

  def countWords(text: String): Int = {
    val textSeq = text.toIndexedSeq

    def count(s: String) = s.length min 1

    foldMapV(textSeq, wcm)(c => if (c.isWhitespace) Part("", 0, "") else Stub(c.toString))
    match {
      case Stub(s) => count(s)
      case Part(l, i, r) => count(l) + i + count(r)
    }
  }

  def mapMergeMonoid[K, V](v: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def zero: Map[K, V] = Map[K, V]()

    override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = {
      (a.keySet ++ b.keySet).foldLeft(zero)((acc, k) =>
        acc.updated(k, v.op(a.getOrElse(k, v.zero), b.getOrElse(k, v.zero))))
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(x: (A, B), y: (A, B)): (A, B) =
      (A.op(x._1, y._1), B.op(x._2, y._2))

    val zero = (A.zero, B.zero)
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def zero: A => B = a => B.zero

    override def op(f: A => B, g: A => B): A => B = a => B.op(f(a), g(a))
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    foldMapV(as, mapMergeMonoid[A, Int](intMonoid))(a => Map(a -> 1))
  }

}

