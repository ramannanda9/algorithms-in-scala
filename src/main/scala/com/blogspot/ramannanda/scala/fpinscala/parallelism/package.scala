package com.blogspot.ramannanda.scala.fpinscala

import java.util.concurrent.{ExecutorService, Future}

package object parallelism {
  type Par[A] = ExecutorService => Future[A]
}
