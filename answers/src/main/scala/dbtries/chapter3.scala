package dbtries

import scala.annotation.tailrec
import scala.language.implicitConversions

/**
  * Created by danbrauer1 on 1/21/17.
  */
class chapter3 {

  // Ex 3.1
  // answer: 3


  /**
    * Ex 3.2
    * An implementation of the 'tail' function—return a given list minus its first entry, aka its 'head'.
    * @param aList the list whose 'tail' you want
    * @tparam T any object
    * @return the 'tail' of the given list
    */
  def myVersionOfTail[T]( aList: List[T]) : List[T] = aList match {
    case _ :: tail => tail
    case List() => throw new UnsupportedOperationException("tail of empty list)")
    case null => throw new NullPointerException("ctail of null list)")
  }


  // trying the above but making it implict...?
  implicit class listDan[T](l: List[T]) {

    def myVersionOfTail : List[T] = l match {
      case _ :: tail => tail
      case List() => throw new UnsupportedOperationException("tail of empty list)")
      case null => throw new NullPointerException("ctail of null list)")
    }

  }
  // not sure I prefer this to a plain old Utils class with statics... but, okay.


  /**
    * Ex 3.3
    * Given a list, replace its head with the given new head.
    */
  def setHead[T]( aList: List[T], newHead: T ) : List[T] = aList match {
    case null => sys.error("NPE ... can't setHead on a null list")
    case Nil => List(newHead)
    case _ :: tail => newHead :: tail
  }


  /**
    * Ex 3.4
    * Remove the first n elements from the given list
    * If list is smaller than n, returns empty list
    */
  def drop[A](l: List[A], n: Int): List[A] = {

    @tailrec
    def loop(l: List[A], countdown: Int) : List[A] = countdown match {
      case x if x < 0 => sys.error(s"drop can't drop a negative number of elements: $x")
      case 0 => l
      case _ => if (l.isEmpty) l else loop( l.tail, countdown-1 )
    }

    loop(l, n)
  }

  /**
    * Ex 3.4 done again with match on list instead of the int
    */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n < 0) sys.error(s"drop can't drop a negative number of elements: $n")
    @tailrec
    def loop(l: List[A], countdown: Int) : List[A] = {
      if (countdown == 0) l
      else l match {
        case Nil => l
        case _ :: tail => loop(tail, countdown - 1)
      }
    }

    loop(l, n)
  }

  /**
    * Ex 3.5
    * First do it with built-in methods
    */
  def dropWhileWithBuiltIns[A](l: List[A], f: A => Boolean): List[A] = {
    l.filter(f)
  }

  /**
    * Ex 3.5 done without built-ins...  NOT tailrec ...
    * Wasn't sure how to do this with tail recursion...  a quick google didn't illuminate so moving on...
    *
    * dropWhile(List(1,2,3,4,5,6,7,8), (x: Int) => x%2==0 )
    *
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {

    def loop(inner: List[A]): List[A] = inner match {
      case List() => inner
      case x :: tail =>
        if (f(x)) x :: loop(tail)
        else loop(tail)
    }

    loop(l)
  }

  /**
    * Ex 3.6
    * drop the last element in a list
    * Again not sure how to do this with tail recursion...
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => l
    case x :: Nil => Nil
    case x :: tail => x :: init(tail)
  }


// TODO I MISREAD THE DEF OF DROPWHILE SO REDO ...
// TODO review book's answers before proceeding and re-read pages 36&37
// TODO ask in #scala for how to do tail-recursive
// TODO see scala's Vector's implementation of head, tail, and init ...

}
