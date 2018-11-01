import RNG.Rand
import org.scalatest.FunSuite


class RNGTest extends FunSuite {
  test("Simple nextInt") {
    val seed = 42
    val rng0 = RNG.Simple(seed)
    val (n1, rng1) = rng0.nextInt
    val (n2, rng2) = rng1.nextInt
    assert(n1 == 16159453)
    assert(n2 == -1281479697)
  }

  test("SimpleRNG nonnegative Int") {
    val seed = 42
    val rng0 = RNG.Simple(seed)
    val (n1, rng1) = RNG.nonnegativeInt(rng0)
    val (n2, rng2) = RNG.nonnegativeInt(rng1)
    assert(n1 == 16159453)
    assert(n2 == 1281479697)
  }

  test("double") {
    val seed = 42
    val rng0 = RNG.Simple(seed)
    val (n1, rng1) = RNG.double(rng0)
    val (n2, rng2) = RNG.double(rng1)
    assert(n1 == 0.007524831686168909)
    assert(n2 == 0.5967354853637516)
  }

  test("intDouble") {
    val seed = 42
    val rng0 = RNG.Simple(seed)
    val (intDbl, r) = RNG.intDouble(rng0)
    assert(intDbl._1 == 16159453)
    assert(intDbl._2 == 0.5967354853637516)
  }

  test("rand List") {
    val seed = 4
    val rng0 = RNG.Simple(seed)
    val (rl, rr) = RNG.ints(5)(rng0)
    val expected = List(1538995, -322738770, 1561020537, -119123850, 358254227)
    assert(rl == expected)
  }

//  test("double2") {
//    val seed = 42
//    val rng0 = RNG.Simple(seed)
//    val (n1, rng1) = RNG.double2(rng0)
//    assert(n1 == 0.007524831686168909)
//    assert(n2 == 0.5967354853637516)
//  }

  test("ints2") {
    val seed = 4
    val rng0 = RNG.Simple(seed)
    val (n1, r2) = rng0.nextInt
    val rl: Rand[List[Int]] = RNG.ints2(5)(rng0)
    val expected = rl(r2)._1
    assert(expected.isInstanceOf[List[Int]])
    assert(expected == List(-322738770, 1561020537, -119123850, 358254227, 784223886, -158151461))
  }

  test("flatMap") {
    val seed = 4
    val rng0 = RNG.Simple(seed)
    val f = (r: RNG) => r.nextInt
    val g = (i: Int) => {(r: RNG) => (i.toDouble / 10, r)}
    val fm = RNG.flatMap(f)(g)
    val d = fm(rng0)._1
    val expected = 153899.5
    assert(d == expected)
  }

  test("nonNegativeLessThan") {
    val seed = 4
    val rng0 = RNG.Simple(seed)
    val maxval = 9
    val fm = RNG.nonNegativeLessThan(maxval)
    val d = fm(rng0)._1
    assert(d < maxval)
  }

  test("map in terms of flatMap") {
    val seed = 4
    val rng0 = RNG.Simple(seed)
    val f = (i: Int) => i.toDouble / 10
    val s = (r: RNG) => r.nextInt
    val m = RNG.map(s)(f)
    val expected = 153899.5
    assert(m(rng0)._1 == expected)
  }


}
