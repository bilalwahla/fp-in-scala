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
}