package dbtries

import fpinscala.errorhandling.Some

/**
  * Created by danbrauer1 on 2/8/17.
  */
class chapter4 {

  /**
    * Ex 4.1
    */
  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(a) => f(a)
      case None => None
    }

    def getOrElse[B>:A](default: => B): B = this match {
      case Some(a) => a
      case None => default
    }

    def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
      case Some(a) => Some(a)
      case None => ob
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if f(a) => Some(a)
      case _ => None
    }
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]


  /**
    * 4.2
    */
  def variance(xs: Seq[Double]): Option[Double] = {
    Some(xs.map( a => math.pow(a - (xs.sum/xs.length) , 2) ).sum / xs.length)
  }
  /// doesn't really work...

  // book's answer
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  /// ????  not helpful...  also they just made up 'mean' without previously definining it...
  // if I'd known mean was a function with an allowed if/else statement, this would've been simpler
  // I thought this all had to be done in a line, with map & flatMap...
  // But I probably still wouldn't have gotten it

  // book's mean
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  /**
    * 4.3
    */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap( a1 => b.map( b1 => f(a1,b1)) )
  }

  //book's answer:
  def map2BOOK[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))



}
