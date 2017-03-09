package dbtries

import fpinscala.laziness.Cons

import scala.annotation.tailrec
import scala.collection.immutable.Stream.cons

/**
  * Created by DanielBrauer on 2/11/17.
  */
class chapter5 {


  import Stream._
  trait Stream[+A] {

    /**
      * Ex 5.1
      */
    def toList: List[A] = {

      @tailrec
      def loop(accum: List[A], remaining: Stream[A]): List[A] = remaining match {
        case Empty => accum
        case Cons (h, t) => loop( h() :: accum, t())
      }

      loop(List(), this)
    }


    def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
      this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
        case _ => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

    @annotation.tailrec
    final def find(f: A => Boolean): Option[A] = this match {
      case Empty => None
      case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
    }

    /**
      * Ex 5.2
      */
    /** my solution is probably not right... I'm sleepy... **/
//    def take(n: Int): Stream[A] = {
//
//      @tailrec
//      def loop(accum: Stream[A], counter: Int, remaining: Stream[A]): Stream[A] = counter match {
//        case `n` => accum
//        case _ => remaining match {
//          case Cons(h,t) => loop( Cons(h, () => accum), counter+1, t() )
//          case Empty => throw new RuntimeException("Not enough to get 'n' entries.  Also this method is poorly written sorry.")
//        }
//      }
//
//      loop(Empty, 0, this)
//    }
    //// the book's answer
//    def take(n: Int): Stream[A] = this match {
//      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
//      case Cons(h, _) if n == 1 => cons(h(), empty)
//      case _ => empty
//    }

    /**
      * Ex 5.2
      * mine's a bit off... though I think it might work...
      */
//    def drop(n: Int): Stream[A] = this match {
//      case Cons(_, t) if n > 0 => t().drop(n - 1)
//      case Cons(_, t) if n == 0 => t()
//      case _ => empty
//    }
    // book's answer:
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }


    // this does not compile ... but not really sure why...
//    def takeWhile(p: A => Boolean): Stream[A] = {
//
//      @tailrec
//      def loop(accum: Stream[A], remaining: Stream[A]): Stream[A] = remaining match {
//        case Empty => accum
//        case Cons (h, t) =>
//          if (p(h())) { loop( Cons(h, accum), t() ) }
//          else { loop (accum, t())}
//      }
//
//      loop(empty, this)
//    }

    // ex 5.4
    // book's answer... not sure this works??  seems like it'll stop going if it hits something that evaluates to false
//    def takeWhile(f: A => Boolean): Stream[A] = this match {
//      case Cons(h,t) if f(h()) => cons(h(), t() takeWhile f)
//      case _ => empty
//    }


    // ex 5.5  ...
//    def takeWhile(f: A => Boolean): Stream[A] = {
//       foldRight(Stream[A]())( (h,t) =>
//        if (f(h)) { cons(h,t) }
//        else empty
//       )
//    }


    // this solution is right according to the book but  Iwonder what happens to the last val?
    def forAll(p: A => Boolean): Boolean = {
      foldRight(false)( (h,t) => p(h) && t ) // what about the last val when t will be Nil?
    }



    // ex 5.6
    // my answer is same as book's except they label t as _ because it doesn't matter
    def headOption: Option[A] = {
      foldRight(Option.empty[A])( (h,t) => Some(h) )
    }



    // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
    // writing your own function signatures.

    // got this one by myself
//    def map[B](f: A => B): Stream[B] = {
//      foldRight(empty[B])( (nextThing, accumulator) => cons(f(nextThing), accumulator ) )
//    }

    // needed help from book's answer
//    def filter(f: A => Boolean): Stream[A] = {
//      foldRight(empty[A])( (next, result) =>  if (f(next)) cons(next, result) else result )
//    }

    // didn't know how to do this one myself...
//    def append(a: A): Stream[A] = {
//      foldRight(empty[A])( (next, result) => if (next == empty) cons(a, result) else result )
//    }
    //book's answer...
//    def append[B>:A](s: => Stream[B]): Stream[B] =
//      foldRight(s)((h,t) => cons(h,t))

    //
    //def flatMap[B](f: A => Stream[B]): Stream[B] = ??
    //book's answer:
//    def flatMap[B](f: A => Stream[B]): Stream[B] =
//    foldRight(empty[B])((h,t) => f(h) append t)





    def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = Stream.cons(1, ones)
    def from(n: Int): Stream[Int] = sys.error("todo")

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
  }


}


object moreCh5Exercises {

  // ex 5.8
  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }

  // book has more 'efficient answer'  ... ?
//  def constant_book[A](a: A): Stream[A] = {
//    lazy val tail: Stream[A] = Cons(() => a, () => tail)
//    tail
//  }

  //ex 5.9
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n+1))
//    def innerFrom(n:Int, add:Int) : Stream[Int] = {
//      Stream.cons(n+add, innerFrom(n, add+1))
//    }
//
//    from(n)
  }

  // ex 5.10
  def fibs(n: Int, priorn: Int): Stream[Int] = {
    val next = n+priorn
    Stream.cons(next, fibs(next, n))
  }
//  val fibs = {
//    def go(f0: Int, f1: Int): Stream[Int] =
//      cons(f0, go(f1, f0+f1))
//    go(0, 1)
//  }

  // ex 5.11
//  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = {
//    val next = f(z)
//    next.map(_ => Stream.cons(_, unfold(_)(f)))
//  }
  // not correct...

  // book's answer...
//  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
//    f(z) match {
//      case Some((h,s)) => cons(h, unfold(s)(f))
//      case None => empty
//    }


}

object ch5unfold {

  // from the book, slightly tweaked
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => Stream.empty
    }

  def constant[A](a: A): Stream[A] = {
    //Stream.cons(a, constant(a))
    //unfold(Stream.empty)( a => Some(a))
    unfold(a)( _ => Some(a,a)) // from book?
  }


//  def map[A, B](as: List[A])(f: A => B): List[B] = {
//    unfold(List[B]())( (a: A) => Some(f(a), as) )   // ????
//  }


  //// this chapter lost me, alas ... will have to return to these topics another day

}



