package dbtries

import fpinscala.laziness.Cons

import scala.annotation.tailrec

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
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

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

    // book's answer... not sure this works??  seems like it'll stop going if it hits something that evaluates to false
    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h,t) if f(h()) => cons(h(), t() takeWhile f)
      case _ => empty
    }



    def forAll(p: A => Boolean): Boolean = sys.error("todo")

    def headOption: Option[A] = sys.error("todo")

    // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
    // writing your own function signatures.

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
