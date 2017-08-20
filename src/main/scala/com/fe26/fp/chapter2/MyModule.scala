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

package com.fe26.fp.chapter2

object MyModule {
  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = if (n <= 0) acc else go(n - 1, acc * n)

    go(n, 1)
  }

  def formatFactorial(x: Int): String = {
    val msg = "The factorial of %d is %d"
    msg.format(x, factorial(x))
  }

  def abs(n: Int): Int = if (n < 0) -n else n

  def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  /**
    * Exercise 2.1: nth fibonacci.
    *
    * @param n number
    * @return nth fibonacci
    */
  def fib(n: Int): Int = {
    def go(n: Int, prev: Int, curr: Int): Int = if (n <= 0) prev else go(n - 1, curr, prev + curr)

    go(n, 0, 1)
  }

  def formatFib(x: Int): String = {
    val msg = "The %dth fibonacci is %d"
    msg.format(x, fib(x))
  }

  // Generalising all format functions using a HOF (Higher order function)
  def formatResult(n: Int, f: Int => Int): String = {
    val msg = "The result of %d is %d"
    msg.format(n, f(n))
  }

  /**
    * Monomorphic function to find a String in an array.
    *
    * @param as array of strings
    * @param key string to look for
    * @return index of first occurrence of key if found, -1 otherwise
    */
  def findFirst(as: Array[String], key: String): Int = {
    def go(i: Int): Int = if (i == as.length) -1 else if (as(i) == key) i else go(i + 1)

    go(0)
  }

  /**
    * Polymorphic function to find an element in an array.
    *
    * @param aa an array
    * @param p search logic
    * @tparam A type of elements in the array
    * @return index of first occurrence of key if found, -1 otherwise
    */
  def findFirst[A](aa: Array[A], p: A => Boolean ): Int = {
    def go(i: Int): Int = if (i == aa.length) -1 else if (p(aa(i))) i else go(i + 1)

    go(0)
  }

  /**
    * Exercise 2.2: checks whether an array is sorted in accordance with given comparison function.
    *
    * @param aa an array
    * @param ordered ordering logic
    * @tparam A type of elements in the array
    * @return true or false. Whether the array is sorted
    */
  def isSorted[A](aa: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(i: Int): Boolean =
      if (i == aa.length - 1) true else if (ordered(aa(i), aa(i + 1))) loop(i + 1) else false

    loop(0)
  }

  /**
    * Higher-order function for performing what’s called partial application.
    */
  def partial[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  /**
    * Exercise 2.3: Currying.
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  /**
    * Exercise 2.4: Uncurrying.
    */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  /**
    * Exercise 2.5: Function composition.
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
