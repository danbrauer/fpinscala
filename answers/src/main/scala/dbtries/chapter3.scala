package dbtries

import fpinscala.datastructures.Cons

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
    *
    * @param aList the list whose 'tail' you want
    * @tparam T any object
    * @return the 'tail' of the given list
    */
  def myVersionOfTail[T](aList: List[T]): List[T] = aList match {
    case _ :: tail => tail
    case List() => throw new UnsupportedOperationException("tail of empty list)")
    case null => throw new NullPointerException("ctail of null list)")
  }


  // trying the above but making it implict...?
  implicit class listDan[T](l: List[T]) {

    def myVersionOfTail: List[T] = l match {
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
  def setHead[T](aList: List[T], newHead: T): List[T] = aList match {
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
    def loop(l: List[A], countdown: Int): List[A] = countdown match {
      case x if x < 0 => sys.error(s"drop can't drop a negative number of elements: $x")
      case 0 => l
      case _ => if (l.isEmpty) l else loop(l.tail, countdown - 1)
    }

    loop(l, n)
  }

  /**
    * Ex 3.4 done again with match on list instead of the int
    */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n < 0) sys.error(s"drop can't drop a negative number of elements: $n")

    @tailrec
    def loop(l: List[A], countdown: Int): List[A] = {
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
    * keepWhile(List(1,2,3,4,5,6,7,8), (x: Int) => x%2==0 )
    *
    */
  def keepWhile[A](l: List[A], f: A => Boolean): List[A] = {

    def loop(inner: List[A]): List[A] = inner match {
      case List() => inner
      case x :: tail =>
        if (f(x)) x :: loop(tail)
        else loop(tail)
    }

    loop(l)
  }

  // Ex 3.5 ... with tail rec... but reverses the list!
  def keepWhile[A](l: List[A], f: A => Boolean): List[A] = {

    @tailrec
    def loop(toFilter: List[A], filtered: List[A]): List[A] = toFilter match {
      case List() => filtered
      case h :: t =>
        if (f(h)) loop(t, h :: filtered)
        else loop(t, filtered)
    }

    loop(l, List())
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

  // Ex 3.6 tailrec version ...  this seems better than the book's answer, which uses mutable buffer... ?
  // except my version needs "reverse" at the end...
  def init[A](l: List[A]): List[A] = {

    @tailrec def loop(accumulatedHead: List[A], reducedTail: List[A]): List[A] = reducedTail match {
      case Nil => accumulatedHead
      case x :: Nil => accumulatedHead
      case x :: tail => loop(x :: accumulatedHead, tail)
    }

    loop(Nil, l).reverse
  }


  /**
    * Ex 3.7
    * Haven't coded this out but I think yes—in the definition of foldRight, you could
    * add a new first line:
    * if (z == 0.0) 0.0  .... or you could do a match/case on z, and then within that the existing
    * match/sase on as.
    *
    * Looked at the answer, turns out I was wrong.  My solution, or some version of it, would work
    * to return the right answer but it would have to go back through all the recursive steps...
    */


  /**
    * Ex 3.8
    */
  //foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_))
  // starts with empty list...
  // for each in the given list, do Cons(next thing in list, the list...)  so...
  //
  //  Cons(1, Cons(2, Cons(3, Nil)))
  //  Cons(1, Cons(2, List(3))
  //  Cons(1, List(2,3))
  //  List(1,2,3)
  //
  // So it just recreates the list?
  // Checked the answer: I m correct but their explanation isn't so clear to me...


  /**
    * Ex 3.9
    */
  def length[A](as: List[A]): Int = {
    as.foldRight(0)((_, count: Int) => count + 1)
  }

  // book's answer (I used Scala standart foldRight, they used the book's own)
  //  def length[A](l: List[A]): Int =
  //    foldRight(l, 0)((_,acc) => acc + 1)

  //val ten = List.fill(10)("hi")
  //length(ten)
  //length(Nil)
  //length(List.fill(34)(1))
  //length(List("a"))


  /**
    * Ex 3.10
    */

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {

    @tailrec
    def loop(inner: List[A], accumulator: B): B = inner match {
      case Nil => accumulator
      case h :: tail => loop(tail, f(accumulator, h))
    }


    loop(as, z)
  }

  // book's answer:   ... same as mine but I didn't realize in this case I could forgo with the inner loop
  //  @annotation.tailrec
  //  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
  //    case Nil => z
  //    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  //  }

  //  foldLeft(List(1,2,3,4,5), 0)(_+_) // should sum to 15
  //  foldLeft(List[Int](), 0)(_+_) // should sum to 0
  //  foldLeft(List[Int](-1,1), 0)(_+_) // should sum to 0
  //  foldLeft(List[Int](10,1,3,10), 1)(_*_) // should be ... 300
  //  foldLeft(List[Int](10,1,-3,10), 1)(_*_) // should be ... -300


  /**
    * ex 3.11
    */

  // sum
  def sum(l: List[Integer]) = foldLeft(l, 0)((b, a) => b + a)

  def sum2(l: List[Integer]) = foldLeft(l, 0)(_ + _)

  // product
  def product(l: List[Integer]) = foldLeft(l, 1)((b, a) => b * a)

  def product2(l: List[Integer]) = foldLeft(l, 1)(_ * _)

  // length
  def length(l: List[Integer]) = foldLeft(l, 0)((b, a) => b + 1)


  // book's answers
  //def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)
  //def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)
  //def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc,h) => acc + 1)


  /**
    * ex 3.12
    */

  def reverse[A](l: List[A]): List[A] = foldLeft(l: List[A], List[A]())((reversed: List[A], next: A) => next :: reversed)

  /**
    * ex 3.13
    */

  // foldLeft in terms of foldRight ... ?
  def foldLeftImpledWithFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as.foldRight(z)((A, B) => f(B, A))
  }

  // ????  this works but I realize I don't actually know what foldRight is ...

  //book's answers
  //  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
  //    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)


  //def foldRightImledWithFoldLeft()   ...
  // got lazy, didn't do this


  /**
    * Ex 3.14
    */

  //  def appendViaFoldLeft[A](l: List[A], appendMe: A): List[A] = {
  //      //l.foldLeft(List())( (accum: List[A], next: A) => accum ++ List(next);  )
  //    // tried a few different things... not sure...
  //
  //  }


  /**
    * Ex 3.15
    */

  def concat[A](l1: List[A], l2: List[A]): List[A] = {
    l2.foldLeft(l1)((accum: List[A], next: A) => accum ::: List[A](next))
  }

  // i guess this is cheating since ::: is really just concat itself... ?
  // try foldRight instead:

  def concat2[A](l1: List[A], l2: List[A]): List[A] = {
    l1.foldRight(l2)((next: A, accum: List[A]) => next :: accum)
  }

  // book's answer:
  //  def concat[A](l: List[List[A]]): List[A] =
  //    foldRight(l, Nil:List[A])(append)


  //  TODO: review diff between foldRight and foldLeft… maybe look online, this book’s not explaining it for me


  /**
    * 3.16
    */

  def plusOne1(l: List[Double]): List[Double] = {
    l.map(a => a + 1)
  }

  def plusOne2(l: List[Double]): List[Double] = {

    @tailrec def loop(newlist: List[Double], oldlist: List[Double]): List[Double] = oldlist match {
      case Nil => newlist
      case h :: t => loop(h + 1 :: newlist, t)
    }

    loop(List(), l).reverse
  }

  def plusOne3(l: List[Double]): List[Double] = {
    l.foldRight(List[Double]())((a, b) => a + 1 :: b)
  }

  /**
    * 3.17
    */

  def convertToString(l: List[Double]): List[String] = {
    l.map(a => a.toString)
  }

  def convertToString2(l: List[Double]): List[String] = {
    l.foldRight(List[String]())((h, t) => h.toString :: t)
  }


  /**
    * 3.18
    */

  def myMap[A, B](as: List[A])(f: A => B): List[B] = {

    @tailrec
    def loop(oldl: List[A], newl: List[B]): List[B] = oldl match {
      case Nil => newl
      case h :: t => loop(t, f(h) :: newl)
    }

    loop(as, List[B]()).reverse
  }


  def myMap2[A, B](as: List[A])(f: A => B): List[B] = {
    as.foldRight(List[B]())((a, b) => f(a) :: b)
  }


  /**
    * 3.19
    */

  def filter1[A](as: List[A])(f: A => Boolean): List[A] = {
    as.foldLeft(List[A]())((newlist, nextA) => if (f(nextA)) nextA :: newlist else newlist).reverse
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    as.foldRight(List[A]())((nextA, newlist) => if (f(nextA)) nextA :: newlist else newlist)
  }

  /**
    * 3.20
    */

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    val list = myMap2(l)(f)
    list.foldRight(List[B]())((res: List[B], next: List[B]) => next ::: res).reverse
  }

  // the book proposes using concat of a map ... I'd forgotten we'd implemented concat...

  /**
    * 3.21
    */

  //def filterViaFlatmap[A](l: List[A])(f: A => Boolean): List[A] = {
  //val f1 = l.foldRight(List[A]())( (next, filtered) => if (f(next)) next::filtered else filtered )
  //l.flatMap( f1 )

  // l.flatMap( a => f(a)) ...

  // ?
  //}

  // book's answer:
  //def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
  //  flatMap(l)(a => if (f(a)) List(a) else Nil)

  // trying again...
  def filterViaFlatmap[A](l: List[A])(f: A => Boolean): List[A] = {
    l.flatMap(a => if (f(a)) List(a) else Nil)
  }





  /**
    * 3.22
    */
  //    def addTogether1[A](l1: List[A], l2: List[A]): List[A] = {
  //
  //      val x = l1.foldLeft((List[A](), l2))
  //         ( ((newlist: List[A], remainingl2: List[A]), nextl1: A) => nextl1+remainingl2.head :: newlist, remainingl2.tail )
  //      x._1
  //
  //    }


  // first try...  won't work because 'head' and 'tail' of empty list throw exceptions...
  def addTogether1(l1: List[Double], l2: List[Double]): List[Double] = {

    @tailrec
    def loop(addedTogether:List[Double], remaining1:List[Double], remaining2:List[Double]):List[Double] = remaining1 match {
      case Nil => addedTogether ::: remaining2
      case h :: t =>
        val added = h + remaining2.head
        loop(added::addedTogether, t, remaining2.tail)
    }

    loop(List[Double](), l1, l2)
  }

  // snuck a peek at book's answer's signature then did this...
  def addTogether2(l1: List[Double], l2: List[Double]): List[Double] = (l1, l2) match {
    case (_, Nil) => l1
    case (Nil, _) => l2
    case (h1::t1,h2::t2) => h1+h2 :: addTogether2( t1, t2 )
  }

  // book's real answer doesn't seem to work here...
  // also I don't think it'll work for lists of un-equal length....
//  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
//    case (Nil, _) => Nil
//    case (_, Nil) => Nil
//    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
//  }

  /**
    * 3.23
    */

  // my version differs a bit from book's in that I append the tail of the longer list to the result...
  def zipWith[A](l1: List[A], l2: List[A])(f: (A,A) => A): List[A] = (l1, l2) match {
    case (a, Nil) => a
    case (Nil, b) => b
    case (h1::t1,h2::t2) => f(h1,h2) :: zipWith(t1,t2)(f)
  }

  // tests...
  // zipWith(List(1,2,3,4),List(10,20,30))(_+_)
  // zipWith(List(1,2,3,4),List(10,20,30,0,50))(_*_)
  // zipWith(List("hi","yes","red","sky"),List("ho","no","blue"))(_ + _)

  

}