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

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  /* Exercise 5.13 */

  def mapUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Empty => None
    case Cons(a, b) => Some((f(a()), b()))
  }

  // My `takeUnfold` solution
  def takeUnfold(n: Int): Stream[A] = unfold(this) {
    case Empty => None
    case Cons(a, b) if n > 1 => Some((a(), b().takeUnfold(n - 1)))
    case Cons(a, _) if n == 1 => Some((a(), empty))
  }

  // `takeUnfold` similar to Book's answer
  def takeUnfold2(n: Int): Stream[A] = unfold((this, n)) {
    case (Empty, _) => None
    case (Cons(a, b), nth) if nth > 1 => Some((a(), (b(), nth - 1)))
    case (Cons(a, _), 1) => Some((a(), (empty, 0)))
  }

  def takeWhileUnfold(f: A => Boolean): Stream[A] = unfold(this) {
    // You could write b().takeWhileUnfold(f) or just b() to leave it to the unfold
    case Cons(a, b) if f(a()) => Some((a(), b()))
    case _ => None
  }

  def zipWithUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(s1h, s1t), Cons(s2h, s2t)) => Some((f(s1h(), s2h()), (s1t(), s2t())))
    // could combine the 3 cases below in to one e.g. `case _ => None`
    case (Empty, Empty) => None
    case (Cons(_, _), Empty) => None
    case (Empty, Cons(_, _)) => None
  }

  def zipAllUnfold[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold(this, s2) {
    case (Cons(s1h, s1t), Cons(s2h, s2t)) => Some(((Some(s1h()), Some(s2h())), (s1t(), s2t())))
    case (Cons(s1h, s1t), Empty) => Some(((Some(s1h()), Option.empty[B]), (s1t(), empty[B])))
    case (Empty, Cons(s2h, s2t)) => Some(((Option.empty[A], Some(s2h())), (empty[A], s2t())))
    case (Empty, Empty) => None
  }

  // Answers from the book suggest generalising `zipAll` to the following
  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(s1h, s1t), Empty) => Some(f(Some(s1h()), Option.empty[B]) -> (s1t(), empty[B]))
      case (Empty, Cons(s2h, s2t)) => Some(f(Option.empty[A], Some(s2h())) -> (empty[A] -> s2t()))
      case (Cons(s1h, s1t), Cons(s2h, s2t)) => Some(f(Some(s1h()), Some(s2h())) -> (s1t() -> s2t()))
    }

  // And then `zipAll` could be simplified to
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = zipWithAll(s2)((_,_))

  /* Exercise 5.14 */
  /*
  `s startsWith s2` when corresponding elements of `s` and `s2` are all equal, until the point that
  `s2` is exhausted. If `s` is exhausted first, or we find an element that doesn't match, we
  terminate early. Using non-strictness, we can compose these three separate logical steps--the
  zipping, the termination when the second stream is exhausted, and the termination if a
  non-matching element is found or the first stream is exhausted.
   */
  def startsWith[B](s2: Stream[B]): Boolean = zipAll(s2).takeWhile(_._2.isDefined).forAll {
    case (Some(a), Some(b)) => a == b
    case _ => false
  }

  /* Exercise 5.15 */
  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(h, t) => Some((cons(h(), t()), cons(h(), t()) drop 1))
    case Empty => None
  } append Stream(empty)

  def hasSubsequence[B](s: Stream[B]): Boolean = tails exists (_ startsWith s)

  /* Exercise 5.16 */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight((z, Stream(z)))((a, p0) => {
    /*
    / `p0` is passed by-name and used in by-name args in `f` and `cons`. So use `lazy val` to
    ensure only one evaluation.
     */
    lazy val p1 = p0
    val b2 = f(a, p1._1)
    (b2, cons(b2, p1._2))
  })._2
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

  /*
  This is more efficient than `cons(a, constant(a))` since it's just one object referencing itself.
   */
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