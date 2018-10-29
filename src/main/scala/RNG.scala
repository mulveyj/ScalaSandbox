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
    def limitedDouble(value: Double, place: Int): Double = {
      value match {
        case k if k == Int.MaxValue => 0.99999999999999
        case k if k >= 1 => limitedDouble(k / 10, place + 1)
        case k if k < 1 => roundDecimal(k, place + 1)
      }
    }

    (limitedDouble(int1.toDouble, 0), rng1)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n1, rng1) = RNG.nonnegativeInt(rng)
    val (n2, rng2) = RNG.double(rng1)
    ((n1, n2), rng2)
  }

  def ints(count: Integer)(rng: RNG): (List[Int], RNG) = {
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


//  helper
  def roundDecimal(number: Double, places: Int): Double = {
    BigDecimal(number).setScale(places, BigDecimal.RoundingMode.HALF_UP).toDouble
  }
}