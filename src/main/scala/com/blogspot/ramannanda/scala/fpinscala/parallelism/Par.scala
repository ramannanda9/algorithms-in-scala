package com.blogspot.ramannanda.scala.fpinscala.parallelism


import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}



case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
  @volatile var cache: Option[C] = None

  override def cancel(mayInterruptIfRunning: Boolean): Boolean = a.cancel(mayInterruptIfRunning) && b.cancel(mayInterruptIfRunning)

  override def isCancelled: Boolean = a.isCancelled || b.isCancelled

  override def isDone: Boolean = cache.isDefined

  private def compute(timeoutNanos: Long): C = cache match {
    case Some(c) => c
    case None =>
      val start = System.nanoTime()
      val ar = a.get(timeoutNanos, TimeUnit.NANOSECONDS)
      val stop = System.nanoTime()
      val timeRemaining = timeoutNanos - (stop - start)
      val br = b.get(timeRemaining, TimeUnit.NANOSECONDS)
      val ret = f(ar, br)
      cache = Some(ret)
      ret
  }

  override def get(): C = compute(Long.MaxValue)

  override def get(timeout: Long, unit: TimeUnit): C = compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

}

object Par {

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es1) => {
      val (af, bf) = (a(es1), b(es1))
      Map2Future(af, bf, f)
    }
  }

  def fork[A](a: => Par[A]): Par[A] = {
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get()
      })
  }

  def lazyUnit[A](a: => A): Par[A] = {
    fork(unit(a))
  }

  def asyncF[A, B](f: A => B): A => Par[B] = {
    (a: A) => lazyUnit(f(a))
  }

  def sortPar[A: Ordering](parList: Par[List[A]]): Par[List[A]] = {
    map2(parList, unit(()))((a, _) => a.sorted)
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
    map2(pa, unit(()))((a, _) => f(a))
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    sequence(ps.map(asyncF(f)))
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequence(t)))(_ :: _)
    }
  }


  def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (ps.isEmpty) unit(Vector())
    else if (ps.length == 1) map(ps.head)(a => Vector(a))
    else {
      val (l, r) = ps.splitAt(ps.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    map(sequence(as map (asyncF((a: A) => if (f(a)) List(a) else List()))))(_.flatten)
  }

  def flatMap[A, B](p: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      run(es)(choices(k))
    }

}
