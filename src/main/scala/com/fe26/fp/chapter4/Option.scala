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

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](obj: => Option[B]): Option[B] = map(Some(_)) getOrElse obj

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  /**
    * Exercise 4.2: If the mean of a sequence is `m`, the variance is the mean of
    * `math.pow(x - m, 2)` for each element `x` in the sequence.
    *
    * @param xs sequence od doubles
    * @return calculated variance as `Option`
    */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  /**
    * Exercise 4.3: a map function to make any existing function of two arguments `Option`-aware.
    *
    * @param a `Option`-ised first argument
    * @param b `Option`-ised second argument
    * @param f function to pass the values of `Option`-ised arguments to
    * @tparam A type of value of the first argument
    * @tparam B type of value of the second argument
    * @tparam C type of value the function `f` returns
    * @return `Option`-ised return value of function `f`
    */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, None) => None
    case (Some(_), None) => None
    case (None, Some(_)) => None
    case (Some(aa), Some(bb)) => Some(f(aa, bb))
  }

  // Lots of trivial cases going on above. Perhaps more readable but not elegant. Lets try again.
  def map2_2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  /**
    * Exercise 4.4: combines a list of `Option`s into one `Option` containing a list of all the
    * `Some` values in the original list. If the original list contains even one `None`, the result
    * of the function should be `None`; otherwise the result should be `Some` with a list of all
    * the values.
    *
    * @param a list to combine
    * @tparam A type of elements
    * @return an `Option` that contains a list of `A`s
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x :: xs => x flatMap(xx => sequence(xs) map(xx :: _))
  }

  // Again lets try without pattern matching
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2_2(x, y)(_ :: _))

  /**
    * Exercise 4.5: combines a list of `Option`s into one `Option` containing a list of all the
    * `Some` values in the original list having applied the given function.
    *
    * @param a list to combine
    * @param f function to apply to each element value
    * @tparam A type of element values in the list
    * @tparam B type element values in the returned result
    * @return an option containing list of all the values after applying the function
    */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => map2_2(f(x), traverse(xs)(f))(_ :: _)
  }

  // Fold
  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2_2(f(x), y)(_ :: _))

  // Sequence in terms of traverse
  def sequence_2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(aa => aa)
}