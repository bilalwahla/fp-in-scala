package com.fe26.fp.chapter4

sealed trait Option[+A] {
  /* Exercise 4.1 */

  /**
    * Apply given function.
    *
    * @param f function to apply on the value
    * @tparam B return type of the argument function
    * @return this after applying the give function on its value
    */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  /**
    * Returns the result inside the `Some` case of the `Option`, or if the `Option` is `None`,
    * returns the given default value.
    *
    * @param default default value to return
    * @tparam B type of the return value
    * @return return value
    */
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B >: A](obj: => Option[B]): Option[B] = map(Some(_)).getOrElse(obj)

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]