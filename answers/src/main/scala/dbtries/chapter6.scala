package dbtries

import fpinscala.state.RNG

/**
  * Created by DanielBrauer on 3/7/17.
  */
class chapter6 {

  // ex 6.1 ... my solution, though I think this doesn't take into account the edge case they mention
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    (Math.abs(i), rng2)
  }

  // book's answer ... with edge case ... they add a 1 to negative numbers...
  def nonNegativeInt2(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }



}
