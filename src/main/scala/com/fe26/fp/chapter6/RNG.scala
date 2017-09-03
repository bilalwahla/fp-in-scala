package com.fe26.fp.chapter6

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
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, rNG2) = rng.nextInt
    if (int >= 0) (int, rNG2) else (-(int + 1), rNG2)
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (int, rnG2) = nonNegativeInt(rng)
    (int / (Int.MaxValue.toDouble + 1), rnG2)
  }

  def doubleRefined: Rand[Double] = rng => {
    val (int, rnG2) = nonNegativeInt(rng)
    (int / (Int.MaxValue.toDouble + 1), rnG2)
  }

  // Exercise 6.3

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, rNG2) = rng.nextInt
    val (d, rNG3) = double(rNG2)
    ((int, d), rNG3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((int, d), rNG2) = intDouble(rng)
    ((d, int), rNG2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rNG2) = double(rng)
    val (d2, rNG3) = double(rNG2)
    val (d3, rNG4) = double(rNG3)
    ((d1, d2, d3), rNG4)
  }

  /* Exercise 6.4 */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(n: Int, ints: List[Int], r: RNG): (List[Int], RNG) = if (n <= count) {
      val (int, r2) = r.nextInt
      loop(n + 1, ints :+ int, r2)
    } else (ints, r)

    loop(1, Nil, rng)
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
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  /*
  There’s also map for transforming the output of a state action without modifying the state
  itself. Remember, `Rand[A]` is just a type alias for a function type `RNG => (A, RNG)`, so this
  is just a kind of function composition.
   */
  def map[A, B](u: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = u(rng)
    (f(a), rng2)
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
}