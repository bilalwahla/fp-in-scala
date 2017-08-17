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

package com.fe26.fp

/**
  * .
  *
  * @author bilalwahla
  */
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

  def fib(n: Int): Int = {
    def go(n: Int, prev: Int, curr: Int): Int = if (n <= 1) prev else go(n - 1, curr, prev + curr)

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
}
