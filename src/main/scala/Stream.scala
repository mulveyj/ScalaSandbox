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

}
