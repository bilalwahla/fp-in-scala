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
}