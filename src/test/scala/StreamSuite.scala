import org.scalatest.{FunSuite, Ignore}

import scala.math._


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

  test("append stream of streams") {
    val sos: Stream[Stream[Int]] = Stream(Stream(1, 2, 3), Stream(1, 2))
    val str: Stream[Stream[Int]] = Stream(Stream(3))
    val appended1: Stream[Stream[Int]] = sos.append[Stream[Int]](str)
    val appended2 = appended1.append(Stream(Stream.empty))
    val expected1 = List(List(1, 2, 3), List(1, 2), List(3))
    val expected2 = List(List(1, 2, 3), List(1, 2), List(3), List.empty)
    assert(appended1.map(l => l.toList).toList == expected1)
    assert(appended2.map(l => l.toList).toList == expected2)
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
    assert(result.contains(4))
  }

  test("constant") {
    val c: Stream[Int] = Stream.constant(3)
    assert(c.take(7).toList == List(3, 3, 3, 3, 3, 3, 3))
  }

  test("from") {
    val f = Stream.from(6)
    assert(f.take(5).toList == List(6, 7, 8, 9, 10))
  }

  test("fibs") {
    val fibnums = Stream.fibs
    assert(fibnums.take(8).toList == List(0, 1, 1, 2, 3, 5, 8, 13))
  }

  test("Unfold"){
    val pi = Pi
    val initial = pi / 6
    val next = (current: Double) => Option((roundDecimal(sin(current), 2), current + pi))

    val angleStream: Stream[Double] = Stream.unfold(initial)(next)

    val expected: List[Double] = List(0.50, -0.50, 0.50, -0.50)

    assert(angleStream.take(4).toList == expected)
  }

  test("fibsUnfold"){
    val fibnums = Stream.fibsUnfold
    assert(fibnums.take(8).toList == List(0, 1, 1, 2, 3, 5, 8, 13))
  }

  test("fromUnfold"){
    val f = Stream.fromUnfold(6)
    assert(f.take(5).toList == List(6, 7, 8, 9, 10))
  }

  test("constantUnfold") {
    val c: Stream[Int] = Stream.constantUnfold(3)
    assert(c.take(7).toList == List(3, 3, 3, 3, 3, 3, 3))
  }

  test("mapUnfold") {
    val c: Stream[Int] = Stream.from(3)
    def f(n: Int): String = (2 * n).toString
    val doubleString = c.mapUnfold(f)
    val expected = List("6", "8", "10", "12")
    assert(doubleString.take(4).toList == expected)
  }

  test("takeUnfold"){
    val c: Stream[Int] = Stream.from(3)
    val first5 = c.takeUnfold(5).toList
    val expected = List(3, 4, 5, 6, 7)
    assert(first5 == expected)
  }

  test("takeWhileUnfold"){
    val c: Stream[Int] = Stream.from(3)
    val numTest = (n: Int) => n < 10
    val lessThan10 = c.takeWhileUnfold(numTest).toList
    val expected = List(3, 4, 5, 6, 7, 8, 9)
    assert(lessThan10 == expected)
  }

  test("zipWith"){
    val a = Stream.constant[Int](4)
    val b = Stream.constant[String]("b")
    def f(arg1: Int, arg2: String):(Int, String) = {
      (arg1, arg2)
    }
    val c = a.zipWith(b, f).take(3).toList
    val expected = List((4, "b"), (4, "b"), (4, "b"))
    assert(c == expected)
  }

  test("zipAll"){
    val numTest = (n: Int) => n < 7
    val a = Stream.from(4).takeWhile(numTest)
    val b = Stream.constant[String]("b")
    val c = a.zipAll(b).take(5).toList
    val expected = List((Some(4), Some("b")), (Some(5), Some("b")), (Some(6), Some("b")), (None, Some("b")), (None, Some("b")))
    assert(c == expected)
  }

  test("zipWith unequal lengths") {
    val a = Stream(5, 6, 7, 8)
    val b = Stream("a", "b", "c")
    def f(arg1: Int, arg2: String):(Int, String) = {
      (arg1, arg2)
    }
    val c = a.zipWith(b, f).take(3).toList
    val expected = List((5, "a"), (6, "b"), (7, "c"))
    assert(c == expected)
  }

  test("startsWith single"){
    val testStream = Stream.from(4)
    val trueStart = Stream(4)
    val falseStart = Stream(7)
    assert(testStream.startsWith(trueStart))
    assert(!testStream.startsWith(falseStart))
  }

  test("startsWith multiple"){
    val testStream = Stream.from(4)
    val trueStart = Stream(4, 5, 6)
    val falseStart = Stream(7, 8, 9, 10)
    assert(testStream.startsWith(trueStart))
    assert(!testStream.startsWith(falseStart))
    assert(Stream(1, 2, 3).startsWith(Stream(1, 2)))
  }

//  test("startsWith null"){
//    val s = Stream(1, 2)
//    assert(Stream.empty.startsWith(s) == false)
//  }

  test("tails"){
    val testStream = Stream(1, 2, 3)
    val t = testStream.tails.map(_.toList).toList
    val expected = List(List(1, 2, 3), List(2, 3), List(3), List())
    assert(t == expected)
  }

//  test("hasSubsequence"){
//    val testStream = Stream(2, 4, 6, 8, 10, 12)
//    val sub1 = Stream(6, 8)
//    val sub2 = Stream(6, 10)
//    assert(testStream.hasSubsequence(sub1))
//    assert(!testStream.hasSubsequence(sub2))
//  }

  def roundDecimal(number: Double, places: Int): Double = {
    BigDecimal(number).setScale(places, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

}
