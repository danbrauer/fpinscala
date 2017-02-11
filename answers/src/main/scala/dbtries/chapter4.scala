package dbtries

import fpinscala.errorhandling.{Left, Right, Some}

import scala.annotation.tailrec
import scala.util.{Left, Right}

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
  def varianceBOOK(xs: Seq[Double]): Option[Double] =
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


  /**
    * 4.4
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {

    def loop(res: List[A], tail: List[Option[A]]): Option[List[A]] = tail match {
      case Nil => Some(res)
      case h :: t => h.flatMap( h1 => loop(h1 :: res, t) )
    }

    loop(List[A](), a)
  }

  // book's answer: ... I forgot what map2 does so this is close to unintelligible...
  def sequence_BOOK[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  /**
    * 4.5
    */
  //  def traverse[A,B](a: List[A])(f:A => Option[B]): Option[List[B]] = {
  //    Some ( a.map( a1 => f(a1).getOrElse(None) ) )
  //  }
  // ??

//  def traverse[A,B](a: List[A])(f:A => Option[B]): Option[List[B]] = {
//      a.foldLeft(List[B]())( (resultList, nextA) => f(nextA).map( b => b :: resultList )   )
//  }

//  def traverse[A,B](a: List[A])(f:A => Option[B]): Option[List[B]] = {
//    a.foldRight(Some(List[B]()))( (nextA, result) => f(nextA) .... ? )
//  }


  /**
    * 4.6
    */

  sealed trait Either[+E,+A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
     }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => b
      case Right(a) => Right(a)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a,b))
    }   // not sure if this is right... keep forgetting what their map2 is supposed to do
  }
  case class Left[+E](get: E) extends Either[E,Nothing]
  case class Right[+A](get: A) extends Either[Nothing,A]

// book's answer's...
//  def map[B](f: A => B): Either[E, B] =
//    this match {
//      case Right(a) => Right(f(a))
//      case Left(e) => Left(e)
//    }
//
//  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
//    this match {
//      case Left(e) => Left(e)
//      case Right(a) => f(a)
//    }
//  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
//    this match {
//      case Left(_) => b
//      case Right(a) => Right(a)
//    }
//  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
//     Either[EE, C] = for { a <- this; b1 <- b } yield f(a,b1)

  /**
    * 4.7
    */

//  def sequence_WITHEITHER[E,A](es: List[Either[E,A]]): Either[E, List[A]] =
//    es.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))
    // ... ran out of time for this...




}
