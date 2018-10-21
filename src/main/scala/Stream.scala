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
}
