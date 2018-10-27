import Stream.empty
import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case _ => None
  }

  def toList: List[A] = {
    @tailrec
    def iterateList(currentList: List[A], remainingStream: Stream[A]): List[A] = {
      remainingStream match {
        case Cons(h, t) => iterateList(h() :: currentList, t())
        case _ => currentList
      }
    }

    iterateList(List.empty[A], this).reverse
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
      case Cons(h, t) if n == 1 => Stream.cons(h(), Stream.empty)
      case _ => Stream.empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _ => Stream.empty
    }
  }

  def exists(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }
  }

  def foldRight[B](acc: => B)(f:(A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(acc)(f))
      case _ => acc
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((item, acc) => p(item) && acc)
  }

  def headOptionFR: Option[A] = {
    foldRight(None: Option[A])((item, acc) => Some(item))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty: Stream[B])((item, acc) => Stream.cons(f(item), acc))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty: Stream[A])((item, acc) => if (p(item)) Stream.cons(item, acc) else acc )
  }

  def append[B>:A](st: Stream[B]): Stream[B] = {
    foldRight(st: Stream[B])((item, acc) => Stream.cons(item, acc))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty: Stream[B])((item, acc) => f(item).append(acc))
  }

  def find(p: A => Boolean): Option[A] = {
    filter(p).headOption
  }

  def mapUnfold[B](f: A => B): Stream[B] = {
    def optionf(a: Stream[A]): Option[(B, Stream[A])] = {
      a match {
        case Cons(h, t) => Some(f(h()), t())
        case _ => None
      }
    }

    Stream.unfold(this)(optionf)
  }

  def takeUnfold(n:Int): Stream[A] = {
    def getNext(v: (Stream[A], Int)): Option[(A, (Stream[A], Int))] = {
      v match {
        case (Cons(h, t), k) if k > 1 => Some(h(), (t(), k - 1))
        case (Cons(h, t), 1) => Some(h(), (Stream.empty[A], 0))
        case _ => None
      }
    }

    Stream.unfold((this, n))(getNext)
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = {
    def getNext(v: Stream[A]): Option[(A, Stream[A])] = {
      v match {
        case Cons(h, t) if p(h()) => Some(h(), t())
        case _ => None
      }
    }
    Stream.unfold(this)(getNext)
  }

  def zipWith[B, C](other: Stream[B], zip: (A, B) => C): Stream[C] = {
    def wrapZip(v: (Stream[A], Stream[B])): Option[(C, (Stream[A], Stream[B]))] = {
      v match {
        case (Empty, Cons(h, t)) => None
        case (Cons(h1, t1), Cons(h2, t2)) => Some(zip(h1(), h2()), (t1(), t2()))
        case _ => None
      }
    }
    Stream.unfold((this, other))(wrapZip)
  }

  def zipAll[B](other: Stream[B]):Stream[(Option[A],Option[B])] = {
    def wrapZip(v: (Stream[A], Stream[B])): Option[((Option[A], Option[B]), (Stream[A], Stream[B]))] = {
      v match {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
        case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Stream.empty))
        case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Stream.empty, t2()))
        case _ => None
      }
    }
    Stream.unfold((this, other))(wrapZip)
  }

  def startsWith[A](start: Stream[A]): Boolean = {
    def zip[B](s1: B, s2: B): Boolean = {
      s1 match {
        case null => false
        case s2 => true
        case _ => false
      }
    }

    zipWith[A, Boolean](start, zip).forAll(identity)
  }

//  def startsWith[A](s: Stream[A]): Boolean =
//    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
//      case (h,h2) => h == h2
//    }

  def tails: Stream[Stream[A]] = {
    def unfolder(s: Stream[A]): Option[(Stream[A], Stream[A])] = {
      s match {
        case Cons(h, t) => Some(s, t())
        case _ => None
      }
    }

    Stream.unfold(this)(unfolder).append(Stream(Stream.empty))
  }

  def hasSubsequence[A](s: Stream[A]): Boolean = {
    tails.exists(p => p.startsWith(s))
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail  = tl
    Cons( () => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = {
    cons[Int](n, from(n + 1))
  }

  def fibs: Stream[Int] = {

    def addNextFib(num0: Int, num1: Int): Stream[Int] = {
        cons(num0, addNextFib(num1, num0 + num1))
    }
    addNextFib(0, 1)
  }

  def unfold[A, S](initialState: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(initialState) match {
        case Some(t:(A, S)) => cons(t._1, unfold(t._2)(f))
        case None => empty[A]
      }
  }

  def fibsUnfold: Stream[Int] = {
    val initialState = (0, 1)
    val f: Function1[(Int, Int), Option[(Int, (Int, Int))]] = { case (n0: Int, n1: Int) => Some(n0, (n1, n0 + n1))}
    unfold(initialState)(f)
  }

  def fromUnfold(n: Int): Stream[Int] = {
    val f = (n: Int) => Some((n, n + 1))
    unfold(n)(f)
  }

  def constantUnfold[A](a: A): Stream[A] = {
    val f = (a: A) => Some((a, a))
    unfold(a)(f)
  }
}
