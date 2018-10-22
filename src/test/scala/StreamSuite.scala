import org.scalatest.FunSuite


class StreamSuite extends FunSuite {
  test("toList"){
    val myStream = Stream(2,3,4,5,6)
    val list = myStream.toList
    assert(list.isInstanceOf[List[Int]])
    assert(list == List(2,3,4,5,6))
  }

  test("take returns Stream stuff") {
    val myStream = Stream(3,4,5,6,7)
    val newStream = Stream(3,4,5)
    val takeResult = myStream.take(3)
    assert(takeResult.toList == newStream.toList)
  }

  test("takeWhile") {
    val myStream = Stream(6, 7, 8, 9, 10, 11, 12)
    val predicate = (n: Int) => n < 10
    val result = myStream.takeWhile(predicate)
    val expected = List(6, 7, 8, 9)
    assert(result.toList == expected)
  }

  test("exists") {
    val myStream = Stream(6, 7, 8, 9, 10, 11, 12)
    val predicate = (n: Int) => n % 11 == 0
    val result = myStream.exists(predicate)
    assert(result)
  }

  test("exists when false") {
    val myStream = Stream(6, 7, 8, 9, 10, 11, 12)
    val predicate = (n: Int) => n % 13 == 0
    val result = myStream.exists(predicate)
    assert(!result)
  }

  test("foldRight") {
    val myStream = Stream(6, 7, 8, 9, 10, 11, 12)
    def addIfEven(n: Int, acc: => Int): Int = {
      n match {
        case k if k % 2 == 0 => acc + k
        case _ => acc
      }
    }
    val result = myStream.foldRight(0)(addIfEven)
    assert(result == 36)
  }

  test("apply exists with foldRight") {
    val myStream = Stream(6, 7, 8, 9, 10, 11, 12)
    val pred = (n: Int) => n % 11 == 0
    def folder(k: Int, b: => Boolean) = {
      pred(k) || b
    }
    val result1 = myStream.foldRight(false)(folder)
    assert(result1)
  }

  test("forAll") {
    val trueStream = Stream(2, 4, 6, 8, 10)
    val falseStream = Stream(2, 4, 6, 9, 10)
    val pred = (n: Int) => n % 2 == 0
    val result1 = trueStream.forAll(pred)
    val result2 = falseStream.forAll(pred)
    assert(result1)
    assert(!result2)
  }

  test("headOption") {
    val fullStream = Stream(6, 7, 8, 9, 10, 11, 12)
    val emptyStream = Stream.empty
    assert(fullStream.headOption.contains(6))
    assert(emptyStream.headOption.isEmpty)
  }

  test("headOptionFR") {
    val fullStream = Stream(6, 7, 8, 9, 10, 11, 12)
    val emptyStream = Stream.empty
    assert(fullStream.headOptionFR.contains(6))
    assert(emptyStream.headOptionFR.isEmpty)
  }

  test("map") {
    val myStream = Stream(6, 7, 8, 9, 10, 11, 12)
    def double(n: Int) = 2 * n
    val result = myStream.map(double)
    val expected = Stream(12, 14, 16, 18, 20, 22, 24)
    assert(result.isInstanceOf[Stream[Int]])
    assert(result.toList == expected.toList)
  }

  test("filter") {
    val trueStream = Stream(2, 3, 4, 5, 6, 8)
    val pred = (n: Int) => n % 2 == 0
    val result1 = trueStream.filter(pred)
    val expected = Stream(2, 4, 6, 8)
    assert(result1.isInstanceOf[Stream[Int]])
    assert(result1.toList == expected.toList)
  }

  test("append") {
    val origStream = Stream(2, 3, 4, 5, 6, 8)
    val appStream = Stream(20, 21, 22)
    val result1 = origStream.append(appStream)
    val expected = Stream(2, 3, 4, 5, 6, 8, 20, 21, 22)
    assert(result1.isInstanceOf[Stream[Int]])
    assert(result1.toList == expected.toList)
  }

  test("flatMap") {
    val origStream = Stream(Stream(1, 2), Stream(3, 4, 5), Stream(6))
    val double = (n: Int) => 2 * n
    def fm(st: Stream[Int]): Stream[Int] = st.map(double)
    val result = origStream.flatMap(fm)
    val expected = Stream(2, 4, 6, 8, 10, 12)
    assert(result.isInstanceOf[Stream[Int]])
    assert(result.toList == expected.toList)
  }

  test("find") {
    val origStream = Stream(2, 3, 4, 5, 6, 8)
    val pred = (n: Int) => n % 4 == 0
    val result = origStream.find(pred)
    assert(result == Some(4))
  }

  test("constant") {
    val c = Stream.constant(3)
    assert(c.take(7).toList == List(3, 3, 3, 3, 3, 3, 3))
  }

  test("from") {
    val f = Stream.from(6)
    assert(f.take(5).toList == List(6, 7, 8, 9, 10))
  }

}
