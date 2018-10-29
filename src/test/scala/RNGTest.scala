import org.scalatest.FunSuite
import scala.math._

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
    assert(n1 == 0.16159453)
    assert(n2 == 0.1281479697)
  }

  test("intDouble") {
    val seed = 42
    val rng0 = RNG.Simple(seed)
    val (intDbl, r) = RNG.intDouble(rng0)
    assert(intDbl._1 == 16159453)
    assert(intDbl._2 == 0.1281479697)
  }

  test("rand List") {
    val seed = 4
    val rng0 = RNG.Simple(seed)
    val (rl, rr) = RNG.ints(5)(rng0)
    val expected = List(1538995, -322738770, 1561020537, -119123850, 358254227)
    assert(rl == expected)
  }
}
