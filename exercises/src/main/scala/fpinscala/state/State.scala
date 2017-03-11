package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
  def nonNegativeInt(rng: RNG): (Int, RNG)
  def double(rng: RNG): (Double, RNG)
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

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

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r) // got from the book
  }

  // ex 6.5
  def double2(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r2) = rng.nextInt
    // ((i, i.toDouble), r2)
    val (d, r3) = rng.double(r2)
    ((i,d), r3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
//    val (d, r2) = rng.double(rng)
//    val (i, r3) = r2.nextInt
//    ((d,i), r3)
    val ((i,d),r) = intDouble(rng)
    ((d,i),r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,r1) = double(rng)
    val (d2,r2) = double(r1)
    val (d3,r3) = double(r2)
    ((d1,d2,d3),r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @tailrec
    def loop(remaining: Int, result: List[Int], nextRng: RNG): (List[Int], RNG)  = remaining match {
      case 0 => (result, nextRng)
      case _ =>
        val (i, r) = nextRng.nextInt
        loop(remaining-1, i::result, r)
    }

    loop(count, List(), rng)
  }


  // ex 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {

    // book's answer... this strikes me as too complicated, difficult to comprehend,
    // the sort of code I don't like writing/reading ...  something more verbose
    // but less "smart" would be more maintainable and workable in a work environment...
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

    // my answer...
    // something like this...  maybe....
//    @tailrec
//    def loop( remainingFs: List[Rand[A]], prevR: RNG, accum : List[A] ) : Rand[List[A]] = remainingFs match {
//      case Nil => rng => (accum, rng) // this is wrong
//      case head :: tail =>
//        val (a, nextR) = head(prevR)
//        loop( tail, nextR, a :: accum)
//    }
//
//    loop(fs, RNG, List[A]())


        // if this were a list of 3 ...
//    r1 => {
//      val (a, r2) = fs(0)(r1)
//      val (b, r3) = fs(1)(r2)
//      val (c, r4) = fs(2)(r3)
//      (List(a,b,c), r4)
//    }

//    r1 => {
//      fs.foldLeft((List[A](), r1))( (b, r) => ( ) ) ... not sure how to do with folding...
//    }


  //}

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
