/*
 * Copyright (c) [2017] [fe26 Limited]
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance
 * with the License.
 *
 * You may obtain a copy of the License at:
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed
 * on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under the License.
 */

package com.fe26.fp.chapter3

/**
  * Sealed means that all implementations of the trait must be declared in this file.
  *
  * `List` data type, parameterised on a type, `A`. + in front of the type parameter signals that A
  * is a covariant.
  *
  * @tparam A
  */
sealed trait List[+A]

/**
  * A `List` data constructor representing the empty list.
  */
case object Nil extends List[Nothing]

/**
  * Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
  * which may be `Nil` or another `Cons`.
  *
  * @param head first element in the list
  * @param tail remaining elements in the list
  * @tparam A
  */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/**
  * `List` companion object. Contains functions for creating and working with lists.
  */
object List {
  /**
    * Variadic function. Meaning it accepts zero or more arguments of type `A`.
    *
    * @param as arguments of type `A`
    * @tparam A
    * @return list made up of passed in arguments
    */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))  // _* is a special type to pass in a `Seq`

  /**
    * A function that uses pattern matching to add up a list of integers.
    *
    * @param ints list of integers
    * @return sum of the list of integers
    */
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0 // The sum of the empty list is 0.
    // The sum of a list starting with `x` is `x` plus the sum of the rest of the list
    case Cons(x, xs) => x + sum(xs)
  }

  /**
    * A function that uses pattern matching to multiply a list of doubles.
    *
    * @param ds list of doubles
    * @return
    */
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  /**
    * Appends the end parameter list to the first.
    *
    * @param a1 list to be appended
    * @param a2 list to append
    * @tparam A
    * @return list with a1 appended with a2
    */
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  /**
    * Exercise 3.2: Removes the first element of the list.
    *
    * @param l list to remove the head of
    * @tparam A type of the elements in the list
    * @return list with its head removed
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  /**
    * Retrieves the first element of the list.
    *
    * @param l list to retrieve from
    * @tparam A type of elements in the list
    * @return first element i.e. head
    */
  def head[A](l: List[A]): A = l match {
    case Nil => sys.error("head of empty list")
    case Cons(h, _) => h
  }

  /**
    * Exercise 3.3: Replacing the value of the first element of the list.
    *
    * @param l list to replace the head of
    * @param h value to replace it with
    * @tparam A type of list elements
    * @return list with a replaced head
    */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setting head of empty list")
    case Cons(_, t) => Cons(h, t)
  }

  /**
    * Exercise 3.4: Removes the first n elements from the list.
    *
    * Assumptions:
    * `<=` rather than `==` is assuming -ve n will drop nothing
    *
    * @param l list to drop elements from
    * @param n number of first elements to drop
    * @tparam A type of elements
    * @return list with first `n` elements dropped
    */
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(h, t) if n <= 0 => Cons(h, t)
    case Cons(_, t) if n > 0 => drop(t, n - 1)
  }

  /**
    * Exercise 3.5: Removes elements from the list prefix as long as they match a predicate.
    *
    * Assumption is that dropping from an empty list here will return Nil.
    *
    * @param l list to remove prefix elements from
    * @param f predicate
    * @tparam A type of the elements of list
    * @return list excluding prefix elements that matched the predicate
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  /**
    * Retrieve nth element in the list.
    *
    * @param i index to retrieve
    * @param l list to retrieve element from
    * @tparam A type of elements in the list
    * @return nth element
    */
  def get[A](i: Int, l: List[A]): A = {
    def go(n: Int, list: List[A]): A = list match {
      case Nil => sys.error("index out of bounds")
      case Cons(h, _) if n == i => h
      case Cons(_, t) => go(n + 1, t)
    }

    go(0, l)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def size[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(_, t) => 1 + size(t)
  }

  /**
    * Exercise 3.9: Compute the length of the list using `foldRight`
    * @param l list to compute the length of
    * @tparam A type of elements in the list
    * @return length
    */
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, y) => y + 1)

  /**
    * Exercise 3.10: general list-recursion function using tail recursion.
    *
    * The purpose of function `f` is to take a value of type `B`, use a list item to modify that
    * value and return it.
    *
    * @param as list to traverse
    * @param z default
    * @param f function to apply
    * @tparam A type of elements in the list
    * @tparam B type of the default
    * @return
    */
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /* Exercise 3.11: Write sum, product, and a function to compute the length of a list using
  `foldLeft`.
   */

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)((y, x) => y + x)

  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1)

  /* End of Exercise 3.11. */

  /**
    * Exercise 3.12: Write a function that returns the reverse of a list. Use a fold.
    *
    * @param l list to reverse
    * @tparam A type of elements in the list
    * @return reversed list
    */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  /**
    * Exercise 3.14: Append in terms of fold. Using `foldRight` as we want to traverse through `l`
    * starting at its end and adding it to the front of the accumulated/resulting list.
    *
    * @param l list to append to
    * @param r list to be be appended
    * @tparam A type of elements of the list
    * @return list representing the two joined
    */
  def append2[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))

  /**
    * Exercise 3.15: concatenates a list of lists into a single list.
    *
    * @param l list to concatenate
    * @tparam A type of elements in the list
    * @return concatenated list
    */
  def concatenate[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  /**
    * Exercise 3.16: transforms a list of integers by adding 1 to each element.
    *
    * @param l list of integers to transform
    * @return transformed integer list
    */
  def addOne(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  /**
    * Exercise 3.16: turns each value in a list of doubles into a String.
    *
    * @param l list of doubles to be transformed
    * @return transformed list of doubles
    */
  def convertToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  /**
    * Exercise 3.18: generalizes modifying each element in a list while maintaining the structure of
    * the list.
    *
    * @param l list to modify
    * @param f function used for modification
    * @tparam A type of elements in the list
    * @tparam B type of elements in the returned list
    * @return transformed/modified list
    */
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  /**
    * Exercise 3.19: removes elements from a list unless they satisfy a given predicate.
    *
    * @param l list to remove elements from
    * @param f predicate for elements not to remove
    * @tparam A type of elements in the list
    * @return list with elements that match the predicate
    */
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  /**
    * Exercise 3.20: `flatMap` that works like `map` except that the function `f` will return a list
    * instead of a single result, and that list should be inserted into the final resulting list.
    *
    * @param l list to flat map
    * @param f function to flat map using
    * @tparam A type of elements in the input list
    * @tparam B type of elements in the returned list
    * @return final list
    */
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = concatenate(map(l)(f))

  /*
  Exercise 3.21: filter using `flatMap`.
   */
  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(x => if (f(x)) List(x) else Nil)
}