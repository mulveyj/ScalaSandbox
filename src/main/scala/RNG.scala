import scala.math._

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL +0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonnegativeInt(rng: RNG): (Int, RNG)  = {
    val (int1, rng1) = rng.nextInt
    int1 match {
      case k if k >= 0 => (int1, rng1)
      case k if k == Int.MinValue => (Int.MaxValue, rng1)
      case _ => (-1 * int1, rng1)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (int1, rng1) = RNG.nonnegativeInt(rng)
    (int1.toDouble / (Int.MaxValue.toDouble + 1), rng1)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n1, rng1) = RNG.nonnegativeInt(rng)
    val (n2, rng2) = RNG.double(rng1)
    ((n1, n2), rng2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def getNext(rng1: RNG, n:Int, l: List[Int]): (List[Int], RNG) = {
      if (n < count) {
        val (num, r) = rng1.nextInt
        val l1 = l ++ List(num)
        getNext(r, n + 1, l1)
      } else {
        (l, rng1)
      }
    }

    getNext(rng, 0, List())
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def double2(rng: RNG): Rand[Double] = {
    map(nonnegativeInt)(num => num / (Int.MaxValue.toDouble + 1))
  }

  def map2[A, B, C](rng1: Rand[A], rng2: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng3) = rng1(rng)
      val (b, rng4) = rng2(rng3)
      (f(a, b), rng4)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = {
    rs.foldRight(unit(List.empty[A]))((elem, acc) => map2(elem, acc)((p1: A, p2: List[A]) => List(p1) ++ p2))
  }

  def ints2(count: Int)(rng: RNG): Rand[List[Int]] = {
    val listOfRNGs = (for (i <- 0 to count) yield (r: RNG) => r.nextInt).toList

    sequence(listOfRNGs)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    (r0: RNG) => {
      val (a, r) = f(r0)
      g(a)(r)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    def retryMod(i: Int) = {
      val mod = i % n
      if (i + n - 1 - mod >= 0)
        RNG.unit(mod)
      else RNG.nonNegativeLessThan(n)
    }
    flatMap(nonnegativeInt)(retryMod)
  }

  def map3[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

//  helper
  def roundDecimal(number: Double, places: Int): Double = {
    BigDecimal(number).setScale(places, BigDecimal.RoundingMode.HALF_UP).toDouble
  }
}