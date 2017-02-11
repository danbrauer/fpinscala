package dbtries

import scala.annotation.tailrec

/**
  * Created by danbrauer1 on 1/14/17.
  */
class chapter2 {

  /**
    * Simple factorial function.
    * Tailr style.
    */
  def factorial(n: Long) : Long = {
    @tailrec def loop(accum: Long, countdown: Long): Long = countdown match {
      case 0 => accum
      case _ => loop(accum * countdown, countdown - 1)
    }

    loop(0, n)
  }

  /**
    * Ex 2.1
    * Method gets the nth fibonacci number, where n=1 means the 1st number.
    * Tail recursive.
    * Take 2 after giving it a tiny bit more thought.  I suspect the book's answer is more concise...
    */
  def fibonacci_tailr2(n: Long): Long = {

    @tailrec
    def loop(minusone: Long, minustwo: Long, counter: Long): Long = n match {
      case x if x < 1 => throw new IllegalArgumentException("n must be greater than 1. n=1 for the 1st fibonacci number. n=2 for the second. etc.")
      case 1 => 0 // first fibonacci number
      case 2 => 1 // second fibonacci number
      case _ if counter < n => loop(minusone+minustwo, minusone, counter+1)
      case _ if counter == n => minusone + minustwo
    }

    loop(1,0,3)
  }

  // Ex 2.1
  // copied from GettingStarted.scala
  // 0 and 1 are the first two numbers in the sequence,
  // so we start the accumulators with those.
  // At every iteration, we add the two numbers to get the next one.
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }

  /**
    * Ex 2.1
    * I wrote this one before reading the chapter.
    * Method gets the nth fibonacci number, where n=1 means the 1st number.
    * Not tail recursive.
    */
  def fibonacci_nontailr(n: Long): Long = n match {
    case x if x < 1 => throw new IllegalArgumentException("n must be greater than 1. n=1 for the 1st fibonacci number. n=2 for the second. etc.")
    case 1 => 0 // first fibonacci number
    case 2 => 1 // second fibonacci number
    case x => fibonacci_nontailr(x-1) + fibonacci_nontailr(x-2)
  }


  /**
    * Ex 2.1
    * I wrote this one before reading the chapter.
    * Method gets the nth fibonacci number, where n=1 means the 1st number.
    * Iterative, not recursive.
    */
  def fibonacci_iterative(n: Long): Long = {

    n match {
      case x if x < 1 => throw new IllegalArgumentException("n must be greater than 1. n=1 for the 1st fibonacci number. n=2 for the second. etc.")
      case 1 => 0 // first fibonacci number
      case 2 => 1 // second fibonacci number
      case _ => {
        var minusone = 1
        var minustwo = 0
        var counter = 3
        while (counter < n) {
          counter += 1
          val tmp = minustwo + minusone
          minustwo = minusone
          minusone = tmp
        }
        minusone + minustwo
      }
    }
  }

  /**
    * Ex 2.2
    */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    if (as==null || as.isEmpty) false; // can't be sorted
    if (as.length == 1) true; // can't be unsorted

    @tailrec
    def loop(n: Int) : Boolean = {
      if (n == as.length-1) true
      else if (ordered(as(n), as(n+1))) loop(n+1)
      else false
    }

    loop(0)
  }

  // helpers for Ex 2.2 and some simple tests
  def orderedInt(a: Int, b: Int): Boolean = { if (b>=a) true else false }
  def orderedChar(a: Char, b: Char): Boolean = { if (b.compareTo(a) >= 0) true else false }
  // isSorted( (1::2::3::4::Nil).toArray, orderedInt )
  // isSorted( (1::2::3::4::2::Nil).toArray, orderedInt )
  // isSorted( ('A'::'B'::'C'::'D'::'E'::Nil).toArray, orderedChar )
  // isSorted( ('A'::'B'::'C'::'F'::'E'::Nil).toArray, orderedChar )
  // isSorted( ('A'::'B'::'C'::'F'::'E'::Nil).toArray, ((a:Char, b:Char) => if (b.compareTo(a) >= 0) true else false) )


  // partial1 puzzle, pg 25-26
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    (b) => f(a, b)
  }
  // ex: partial1(10, (a:Int,b:Int)=> a==b)
  // resX(10) ... should be true
  // resX(11) ... should be false
  // trying again with explicit typing:
//  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
//    (b: B) => f(a: A, b: B)
//  }

  /**
    * Ex 2.3
    * I think I got this right but... brain hurts
    */
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a,b)
  }
  // to test:
  // val testf = (a: Int, b: Int) => (a*b)
  // curry(testf)  ... returns something like res1: Int => (Int => Int) = <function1>
  // res1(10) ... should return (B => C) ... res2: Int => Int ...
  // res2(10) = 100

  // the book's answer:
  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
//  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
//  a => b => f(a, b)

  /**
    * Ex 2.4
    */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A,b: B) => {
      val bToC: B => C = f(a)
      bToC(b)
    }
  }

  /**
    * Ex 2.4, shorter version.
    * If I ever met someone who wrote code like this in a professional setting, I'd wish them harm.
    */
  def uncurryShorter[A,B,C](f: A => B => C): (A, B) => C = {
    (a,b) => f(a)(b)
  }

  // book's answer for 2.4:
  // Exercise 4: Implement `uncurry`
//  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
//  (a, b) => f(a)(b)

  /**
    * Ex 2.5
    */
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

}
