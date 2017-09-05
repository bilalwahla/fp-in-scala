package com.fe26.fp.chapter6

import com.fe26.fp.chapter6.RNG.Rand

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL

      // The next state, which is an `RNG` instance created from the new seed.
      val nextRNG = Simple(newSeed)

      // `>>>` is right binary shift with zero fill. The value `n` is new pseudo-random integer.
      val n = (newSeed >>> 16).toInt

      // The return value is a tuple containing both a pseudo-random integer and next `RNG` state.
      (n, nextRNG)
    }
  }

  // Exercise 6.1: `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
  def nonNegativeInt(rNG: RNG): (Int, RNG) = {
    val (int, rNG2) = rNG.nextInt
    if (int >= 0) (int, rNG2) else (-(int + 1), rNG2)
  }

  // Exercise 6.2
  def double(rNG: RNG): (Double, RNG) = {
    val (int, rnG2) = nonNegativeInt(rNG)
    (int / (Int.MaxValue.toDouble + 1), rnG2)
  }

  def doubleRefined: Rand[Double] = rNG => {
    val (int, rnG2) = nonNegativeInt(rNG)
    (int / (Int.MaxValue.toDouble + 1), rnG2)
  }

  // Exercise 6.3

  def intDouble(rNG: RNG): ((Int,Double), RNG) = {
    val (int, rNG2) = rNG.nextInt
    val (d, rNG3) = double(rNG2)
    ((int, d), rNG3)
  }

  def doubleInt(rNG: RNG): ((Double,Int), RNG) = {
    val ((int, d), rNG2) = intDouble(rNG)
    ((d, int), rNG2)
  }

  def double3(rNG: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rNG2) = double(rNG)
    val (d2, rNG3) = double(rNG2)
    val (d3, rNG4) = double(rNG3)
    ((d1, d2, d3), rNG4)
  }

  /* Exercise 6.4 */
  def ints(count: Int)(rNG: RNG): (List[Int], RNG) = {
    def loop(n: Int, ints: List[Int], r: RNG): (List[Int], RNG) = if (n <= count) {
      val (int, r2) = r.nextInt
      loop(n + 1, ints :+ int, r2)
    } else (ints, r)

    loop(1, Nil, rNG)
  }

  /*
  Each of our functions has a type of the form `RNG => (A, RNG)` for some type `A`.

  Functions of this type are called state actions or state transitions because they transform `RNG`
  states from one to the next.
   */
  type Rand[+A] = RNG => (A, RNG)

  /*
  We can now turn methods such as `RNG`'s nextInt into values of this new type.
   */
  def int: Rand[Int] = _.nextInt

  /*
  We want to write combinators that let us combine `Rand` actions while avoiding explicitly passing
  along the `RNG` state. We’ll end up with a kind of domain-specific language that does all of the
  passing for us. For example, a simple `RNG` state transition is the `unit` action, which passes
  the `RNG` state through without using it, always returning a constant value rather than a random
  value.
   */
  def unit[A](a: A): Rand[A] = rNG => (a, rNG)

  /*
  There’s also map for transforming the output of a state action without modifying the state
  itself. Remember, `Rand[A]` is just a type alias for a function type `RNG => (A, RNG)`, so this
  is just a kind of function composition.
   */
  def map[A, B](u: Rand[A])(f: A => B): Rand[B] = rNG => {
    val (a, rNG2) = u(rNG)
    (f(a), rNG2)
  }

  /* Exercise 6.5 */
  val randDouble: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  /* Exercise 6.6 */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rNG => {
    val (a, rNG2) = ra(rNG)
    val (b, rNG3) = rb(rNG2)
    (f(a, b), rNG3)
  }

  /*
  We only have to write the `map2` combinator once, and then we can use it to combine arbitrary
  RNG state actions. For example, if we have an action that generates values of type `A` and an
  action to generate values of type `B`, then we can combine them into one action that generates
  pairs of both `A` and `B`.
   */
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /*
  Exercise 6.7: Combine a whole list of `RNG` transitions.
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  // Nesting state actions

  def nonNegativeLessThanSkewed(n: Int): Rand[Int] = map(nonNegativeInt) { _ % n }

  /*
  Above will certainly generate a number in the range, but it’ll be skewed because `Int.MaxValue`
  may not be exactly divisible by `n`. So numbers that are less than the remainder of that division
  will come up more frequently. When `nonNegativeInt` generates numbers higher than the largest
  multiple of `n` that fits in a 32-bit integer, we should retry the generator and hope to get a
  smaller number.

  But `nonNegativeLessThan(n)` has the wrong type to be used right there. Remember, it should return
  a `Rand[Int]` which is a function that expects an `RNG`! But we don’t have one right there. What
  we would like is to chain things together so that the RNG that’s returned by `nonNegativeInt` is
  passed along to the recursive call to `nonNegativeLessThan`. We could pass it along explicitly
  instead of using `map`
   */
  def nonNegativeLessThan(n: Int): Rand[Int] = { rNG =>
    val (int, rNG2) = nonNegativeInt(rNG)
    val mod = int % n
    if (int + (n - 1) - mod >= 0)
      (mod, rNG2)
    else nonNegativeLessThan(n)(rNG2)
  }

  /* Exercise 6.8 */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rNG => {
    val (a, rNG2) = f(rNG)
    g(a)(rNG2)
  }

  def nonNegativeLessThanFlatMap(n: Int): Rand[Int] = flatMap(nonNegativeInt) { int =>
    val mod = int % n
    if (int + (n - 1) - mod >= 0)
      unit(mod)
    else nonNegativeLessThanFlatMap(n)
  }

  /* Exercise 6.9 */

  def mapUsingFlatMap[A, B](u: Rand[A])(f: A => B): Rand[B] = flatMap(u)(a => unit(f(a)))

  def map2UsingFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def both2[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2UsingFlatMap(ra, rb)((_, _))

  val randIntDouble2: Rand[(Int, Double)] = both2(int, double)
}

// A general state action data type

import State._  // This is to be able to use State companion object

case class State[S, +A](run: S => (A, S)) {
  /* Exercise 6.10 */

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}