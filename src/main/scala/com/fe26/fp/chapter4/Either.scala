package com.fe26.fp.chapter4

sealed trait Either[+E, +A] {
  /* Exercise 4.6 */

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  // Using pattern matching
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Left(e1), Left(_)) => Left(e1)
    case (Left(e), Right(_)) => Left(e)
    case (Right(_), Left(e)) => Left(e)
    case (Right(a1), Right(a2)) => Right(f(a1, a2))
  }

  // map2 using flatMap and map
  def map3[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap (aa => b map (bb => f(aa, bb)))

  // map2 using for-comprehension
  def map4[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    aa <- this
    bb <- b
  } yield f(aa, bb)
}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!") else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = Try(x / y)

  def Try[A](a: => A): Either[Exception, A] = try Right(a) catch {
    case e: Exception => Left(e)
  }

  /* Exercise 4.7 */

  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = traverse(a)(aa => aa)

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    a.foldRight[Either[E, List[B]]](Right(Nil))((x, y) => f(x).map3(y)(_ :: _))
}