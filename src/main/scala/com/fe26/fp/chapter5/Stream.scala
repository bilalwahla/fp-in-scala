package com.fe26.fp.chapter5

import Stream._
sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  /* Exercise 5.1 */
  def toList: List[A] = {
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => loop(t(), h() :: acc)
    }

    loop(this, Nil).reverse
  }

  /* Exercise 5.2 */

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /* Exercise 5.3 */
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def exists2(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  /* Exercise 5.4 */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  /* Exercise 5.5 */
  def takeWhile2(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else empty)

  /* Exercise 5.6 */
  def headOption2: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  /* Exercise 5.7 */

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] = filter(p).headOption
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

case object Empty extends Stream[Nothing]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  /**
    * `Empty` as a `Stream[A]` for type inference. Scala uses subtyping to represent data
    * constructors, but we almost always want to infer `Stream` as the type, not `Cons` or `Empty`.
    * Making smart constructors that return the base type is a common trick.
    *
    * @tparam A even though an empty stream defining type of type inference
    * @return
    */
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /* Exercise 5.8 */

  def ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /* This is more efficient than `cons(a, constant(a))` since it's just one object referencing itself.*/
  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  /**
    * Exercise 5.9: In Scala, the Int type is a 32-bit signed integer, so this stream will switch
    * from positive to negative values at some point, and will repeat itself after about four
    * billion elements.
    *
    * @param n starting number
    * @return stream of integers
    */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /* Exercise 5.10 */
  def fibs: Stream[Int] = {
    def loop(prev: Int, last: Int): Stream[Int] = cons(prev, loop(last, prev + last))

    loop(0, 1)
  }

  /**
    * Exercise 5.11: It takes an initial state, and a function for producing both the next state and
    * the next value in the generated stream
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  /*
  The below two implementations use `fold` and `map` functions in the `Option` class to implement
  `unfold`, thereby doing away with the need to manually pattern match as in the above solution.
   */

  def unfold2[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map((p: (A, S)) => cons(p._1, unfold2(p._2)(f))).getOrElse(empty[A])

  def unfold3[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(empty[A])((p: (A, S)) => cons(p._1, unfold3(p._2)(f)))

  /* Exercise 5.12 */

  def ones2: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def constant3[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  /*
  Scala provides shorter syntax when the first action of a function literal is to match on an
  expression. The function passed to `unfold` in `fibs2` is equivalent to
  `p => p match { case (f0,f1) => ... }`, but we avoid having to choose a name for `p`, only to
  pattern match on it.
  */
  def fibs2: Stream[Int] = unfold((0, 1)) {
    case (prev, last) => Some((prev, (last, prev + last)))
  }

  def from2(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))
}