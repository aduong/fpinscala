package fpinscala.laziness

import org.scalatest.{FunSpec,Matchers}
import Function.const

class StreamSpec extends FunSpec with Matchers {

  def explode[A]: Stream[A] = throw new Exception("Not Lazy!")

  @annotation.tailrec
  final def elem[A](s: Stream[A], n: Int): Option[A] = (s, n) match {
    case (Cons(h, _), 0) => Some(h())
    case (Cons(_, t), n) => elem(t(), n - 1)
    case (Empty, _) => None
  }

  def allEqual[A](x: A, s: Stream[A], n: Int): Boolean = {
    val firstN = for {
      i <- (0 until n).view
    } yield elem(s, i) getOrElse x

    firstN forall { _ == x }
  }


  def testFrom(from: Int => Stream[Int]): Unit = {
    it("should look like a subsequence of the naturals") {
      val s0 = from(0)
      (0 to 100).forall { i =>
        i == elem(s0, i).getOrElse(i)
      } shouldBe true
    }
  }

  describe("from1") { testFrom(Stream.from1) }
  describe("from2") { testFrom(Stream.from2) }
  describe("from") { testFrom(Stream.from) }

  def testConstant(constant: Any => Stream[Any]): Unit = {
    it("should always produce the same thing") {
      allEqual(0, constant(0), 100) shouldBe true
      allEqual("hello", constant("hello"), 100) shouldBe true
    }
  }

  describe("constant1") { testConstant(Stream.constant1) }
  describe("constant2") { testConstant(Stream.constant2) }
  describe("constant") { testConstant(Stream.constant) }

  trait Find {
    def find[A]: (Stream[A], A => Boolean) => Option[A]
  }

  def testFind(f: Find): Unit = {
    import f.find
    it("should find nothing in empty stream") {
      find(Stream.empty, const(true)) shouldBe None
    }
    it("should find things in a finite stream") {
      find(Stream(1, 2), const(true)) shouldBe Some(1)
      find(Stream(1, 2), { 2 == (_: Int) }) shouldBe Some(2)
      find(Stream(1, 2), const(false)) shouldBe None
    }
    it("should find things in an infinite stream") {
      find(Stream.ones, const(true)) shouldBe Some(1)
      find(Stream.ones, { 1 == (_: Int)}) shouldBe Some(1)
    }
  }

  describe("find1") { testFind(new Find { def find[A] = _ find1 _ }) }
  describe("find2") { testFind(new Find { def find[A] = _ find2 _ }) }
  describe("find") { testFind(new Find { def find[A] = _ find _ }) }

  def testTake(take: (Stream[_], Int) => Stream[_]): Unit = {
    it("should take nothing") {
      take(Stream(1, 2, 3), 0) shouldBe Stream.empty
      take(Stream.empty, 0) shouldBe Stream.empty
    }
    it("should take something") {
      take(Stream(1, 2, 3), 2).toList shouldBe List(1, 2)
    }
    it("should take everything") {
      take(Stream(1, 2), 3).toList shouldBe List(1, 2)
    }
    it("should take only a few in an infinite stream") {
      take(Stream.ones, 2).toList shouldBe List(1, 1)
      take(Stream.from(0), 2).toList shouldBe List(0, 1)
    }
  }

  describe("take1") { testTake(_ take1 _ ) }
  describe("take2") { testTake(_ take2 _ ) }
  describe("take3") { testTake(_ take3 _ ) }
  describe("take") { testTake(_ take _ ) }

  describe("drop") {
    it("should drop 0") {
      (Stream(1, 2, 3, 4) drop 0).toList shouldBe List(1, 2, 3, 4)
    }
    it("should drop some items") {
      (Stream(1, 2, 3, 4) drop 2).toList shouldBe List(3, 4)
      (Stream(1, 2, 3, 4) drop 4) shouldBe Stream.empty
    }
    it("should drop everything") {
      Stream(1, 2) drop 4 shouldBe Stream.empty
    }
  }

  describe("forAll") {
    it("should work in the happy path") {
      Stream(1, 2, 3) forAll const(true) shouldBe true
      Stream(1, 2, 3) forAll const(false) shouldBe false
      Stream(1, 2) forAll { _ < 3 } shouldBe true
      Stream(1, 2, 3) forAll { _ < 3 } shouldBe false
    }
    it("should short-circuit") {
      Stream.ones forAll const(false) shouldBe false
      Stream.from(0) forAll { _ < 3 } shouldBe false
    }
  }

  trait TakeWhile {
    def takeWhile[A]: (Stream[A], A => Boolean) => Stream[A]
  }

  def testTakeWhile(t: TakeWhile): Unit = {
    import t.takeWhile
    it("should return empty on the empty stream") {
      takeWhile(Stream.empty, const(true)) shouldBe Stream.empty
      takeWhile[Int](Stream.empty, { _ < 3 }) shouldBe Stream.empty
    }
    it("should return everything on all-true predicates") {
      takeWhile(Stream(1, 2, 3), const(true)).toList shouldBe List(1, 2, 3)
      takeWhile[Int](Stream(1, 2, 3), { _ < 4 }).toList shouldBe List(1, 2, 3)
    }
    it("shoould return nothing on all-false predicates") {
      takeWhile[Int](Stream(1, 2, 3), { _ < 0 }) shouldBe Stream.empty
      takeWhile(Stream(1, 2, 3), const(false)) shouldBe Stream.empty
    }
    it("should return only prefixes which are acceptable") {
      takeWhile[Int](Stream(1, 2, 3, 2), { _ < 3 }).toList shouldBe List(1, 2)
    }
    it("should short-circuit") {
      takeWhile(Stream.ones, const(false)) shouldBe Stream.empty
      takeWhile[Int](Stream.ones, { _ < 0 }) shouldBe Stream.empty
      takeWhile[Int](Stream.from(0), { _ < 3 }).toList shouldBe List(0, 1, 2)
    }
  }

  describe("takeWhile1") { testTakeWhile(new TakeWhile { def takeWhile[A] = _ takeWhile1 _ }) }
  describe("takeWhile2") { testTakeWhile(new TakeWhile { def takeWhile[A] = _ takeWhile2 _ }) }
  describe("takeWhile") { testTakeWhile(new TakeWhile { def takeWhile[A] = _ takeWhile _ }) }

  describe("zip") {
    it("should yield empty on any empty streams") {
      Stream.empty zip Stream.empty shouldBe Stream.empty
      Stream.empty zip Stream(1) shouldBe Stream.empty
      Stream(1) zip Stream.empty shouldBe Stream.empty
    }
    it("should zip together equal-sized streams") {
      (Stream(1) zip Stream(1)).toList shouldBe List((1, 1))
      (Stream(1, 2) zip Stream(1, 2)).toList shouldBe List((1, 1), (2, 2))
    }
    it("should zip up to the smallest stream") {
      (Stream(1) zip Stream(1, 2)).toList shouldBe List((1, 1))
      (Stream(1, 2) zip Stream(1)).toList shouldBe List((1, 1))
    }
    it("should be lazy") {
      (Stream.from(0) zip Stream.from(0) take 0) shouldBe Stream.empty
      (Stream.from(0) zip Stream.from(0) take 2).toList shouldBe List((0, 0), (1, 1))
    }
  }

  trait ZipWith {
    def zipWith[A,B,C]: (Stream[A], Stream[B]) => ((A, B) => C) => Stream[C]
  }

  def testZipWith(z: ZipWith): Unit = {
    import z.zipWith
    it("should combine streams with the given function") { // mostly covered by zip above
      zipWith(Stream(1, 2), Stream(1, 2))(_ + _).toList shouldBe List(2, 4)
      (zipWith(Stream.from(0), Stream.from(0))( _ + _ ) take 3).toList shouldBe List(0, 2, 4)
    }
  }

  describe("zipWith1") { testZipWith(new ZipWith { def zipWith[A,B,C] = _ zipWith1 _ }) }
  describe("zipWith2") { testZipWith(new ZipWith { def zipWith[A,B,C] = _ zipWith2 _ }) }
  describe("zipWith") { testZipWith(new ZipWith { def zipWith[A,B,C] = _ zipWith _ }) }

  def testHeadOption(headOption: Stream[_] => Option[_]): Unit = {
    it("should return none on empty") {
      headOption(Stream.empty) shouldBe None
    }
    it("should return something on non-empty") {
      headOption(Stream(1)) shouldBe Some(1)
    }
    it("should handle infinite streams") {
      headOption(Stream.from(0)) shouldBe Some(0)
    }
  }

  describe("headOption1") { testHeadOption(_.headOption1) }
  describe("headOption2") { testHeadOption(_.headOption2) }
  describe("headOption") { testHeadOption(_.headOption) }

  trait Map {
    def map[A,B]: (Stream[A], A => B) => Stream[B]
  }

  def testMap(m: Map): Unit = {
    import m.map
    it("should map empty to empty") {
      map(Stream.empty, identity) shouldBe Stream.empty
      map(Stream.empty, (_: Int) + 1) shouldBe Stream.empty
    }
    it("should map finite streams correctly") {
      map(Stream(1, 2, 3), identity[Int]).toList shouldBe List(1, 2, 3)
      map(Stream(1, 2, 3), (_: Int) + 1).toList shouldBe List(2, 3, 4)
    }
    it("should map infinite streams lazily") {
      map(Stream.ones, identity[Int]) take 0 shouldBe Stream.empty
      (map(Stream.ones, identity[Int]) take 2).toList shouldBe List(1, 1)
      (map(Stream.ones, (_: Int) + 1) take 2).toList shouldBe List(2, 2)
    }
  }

  describe("map1") { testMap(new Map { def map[A,B] = _ map1 _ }) }
  describe("map2") { testMap(new Map { def map[A,B] = _ map2 _ }) }
  describe("map3") { testMap(new Map { def map[A,B] = _ map3 _ }) }
  describe("map") { testMap(new Map { def map[A,B] = _ map _ }) }

  describe("append") {
    it("should treat empty as identity") {
      Stream.empty append Stream.empty shouldBe Stream.empty
      (Stream.empty append Stream(1)).toList shouldBe List(1)
      (Stream(1) append Stream.empty).toList shouldBe List(1)
    }
    it("should properly append two finite streams") {
      (Stream(1, 2) append Stream(3, 4)).toList shouldBe List(1, 2, 3, 4)
    }
    it("should properly append an infinite stream to a finite stream") {
      (Stream.empty append Stream.from(0) take 5).toList shouldBe List(0, 1, 2, 3, 4)
      (Stream(0) append Stream.from(1) take 5).toList shouldBe List(0, 1, 2, 3, 4)
    }
    it("should properly append two infinite streams...") {
      (Stream.ones append Stream.ones take 5).toList shouldBe List(1, 1, 1, 1, 1)
      (Stream.ones append Stream.from(0) take 5).toList shouldBe List(1, 1, 1, 1, 1)
    }
    it("should be lazy in its second argument") {
      (Stream.ones append explode take 5).toList shouldBe List(1, 1, 1, 1, 1)
    }
  }

  describe("filter") {
    it("should return empty on empty") {
      Stream.empty filter const(true) shouldBe Stream.empty
      Stream.empty[Int] filter { _ < 0 } shouldBe Stream.empty
    }
    it("should properly filter finite streams") {
      (Stream(0, 1, 2, 3) filter const(true)).toList shouldBe List(0, 1, 2, 3)
      Stream(0, 1, 2, 3) filter const(false) shouldBe Stream.empty
      (Stream(0, 1, 2, 3) filter { _ % 2 == 0 }).toList shouldBe List(0, 2)
    }
    it("should properly and lazily filter infinite streams") {
      (Stream.ones filter const(true) take 5).toList shouldBe List(1, 1, 1, 1, 1)
      (Stream.from(0) filter { _ % 2 == 0 } take 5).toList shouldBe List(0, 2, 4, 6, 8)
      (Stream.from(0) filter { _ > 50 } take 2).toList shouldBe List(51, 52)
    }
  }

  describe("flatMap") {
    it("should return empty for empty streams") {
      Stream.empty flatMap Stream.apply shouldBe Stream.empty
      Stream.empty flatMap const(Stream(1)) shouldBe Stream.empty
    }

    it("should properly handle finite streams") {
      Stream(1, 2) flatMap const(Stream.empty) shouldBe Stream.empty
      (Stream(1, 2) flatMap { Stream(_) }).toList shouldBe List(1, 2)
      (Stream(1, 2) flatMap const(Stream(1))).toList shouldBe List(1, 1)
    }

    it("should handle infinite streams lazily") {
      (Stream.ones flatMap { Stream(_) } take 5).toList shouldBe List(1, 1, 1, 1, 1)
      (Stream.from(0) flatMap Stream.from take 3).toList shouldBe List(0, 1, 2)
    }
  }

  describe("unfold") {
    it("should terminate on none") {
      Stream.unfold(())(const(None)) shouldBe Stream.empty
    }
    it("should produce infinite ones") {
      (Stream.unfold(())(const(Some((1, ())))) take 5).toList shouldBe List(1, 1, 1, 1, 1)
    }
  }

  def testFibs(fibs: Stream[Int]): Unit = {
    val expected = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    it(s"first ${expected.length} should be correct") {
      (fibs take expected.length).toList shouldBe expected
    }
  }

  describe("fibs1") { testFibs(Stream.fibs1) }
  describe("fibs2") { testFibs(Stream.fibs2) }
  describe("fibs") { testFibs(Stream.fibs) }

  def testZipAll(zipAll: (Stream[Any], Stream[Any]) => Stream[(Option[Any], Option[Any])]): Unit = {
    it("should zip equal length finite streams properly") {
      zipAll(Stream.empty, Stream.empty) shouldBe Stream.empty
      zipAll(Stream(1), Stream(1)).toList shouldBe List((Some(1), Some(1)))
    }
    it("should zip unequal length finite streams properly") {
      zipAll(Stream(1), Stream.empty).toList shouldBe List((Some(1), None))
      zipAll(Stream.empty, Stream(1)).toList shouldBe List((None, Some(1)))
      zipAll(Stream(1), Stream(1, 2)).toList shouldBe List((Some(1), Some(1)), (None, Some(2)))
    }
    it("should zip infinite streams properly") {
      (zipAll(Stream.ones, Stream.ones) take 10).toList forall { _ == (Some(1), Some(1)) } shouldBe true
      (zipAll(Stream.ones, Stream.empty) take 10).toList forall { _ == (Some(1), None) } shouldBe true
      (zipAll(Stream.empty, Stream.ones) take 10).toList forall { _ == (None, Some(1)) } shouldBe true
    }
  }

  describe("zipAll1") { testZipAll(_ zipAll1 _) }
  describe("zipAll2") { testZipAll(_ zipAll2 _) }
  describe("zipAll3") { testZipAll(_ zipAll3 _) }
  describe("zipAll") { testZipAll(_ zipAll _) }

  def testStartsWith(startsWith: (Stream[Any], Stream[Any]) => Boolean): Unit = {
    it("every stream starts with the empty stream") {
      startsWith(Stream.empty, Stream.empty) shouldBe true
      startsWith(Stream(1), Stream.empty) shouldBe true
      startsWith(Stream.ones, Stream.empty) shouldBe true
    }
    it("the empty stream starts with no stream (except the empty stream)") {
      startsWith(Stream.empty, Stream(1)) shouldBe false
      startsWith(Stream.empty, Stream.ones) shouldBe false
    }
    it("various finite streams") {
      startsWith(Stream(1, 2), Stream(1)) shouldBe true
      startsWith(Stream(1), Stream(1, 2)) shouldBe false
    }
    it("various infinite streams with finite streams") {
      startsWith(Stream.ones, Stream(1, 1)) shouldBe true
      startsWith(Stream.ones, Stream(2)) shouldBe false
      startsWith(Stream.from(0), Stream(0, 1, 2, 3)) shouldBe true
    }
  }

  describe("startsWith") { testStartsWith(_ startsWith _) }

  def testTails(tails: Stream[Any] => Stream[Stream[Any]]): Unit = {
    it("should never yield an empty stream") {
      tails(Stream.empty).toList shouldBe List(Stream.empty)
    }
    it("should handle finite lists properly") {
      tails(Stream(1, 2)).map(_.toList).toList shouldBe List(List(1, 2), List(2), List())
    }
    it("should handle infinite lists properly") {
      val ts = (tails(Stream.from(0)) take 3).toList
      ts.length shouldBe 3
      (ts(0) take 3).toList shouldBe List(0, 1, 2)
      (ts(1) take 3).toList shouldBe List(1, 2, 3)
      (ts(2) take 3).toList shouldBe List(2, 3, 4)
    }
  }

  describe("tails1") { testTails(_.tails1) }
  describe("tails2") { testTails(_.tails2) }
  describe("tails3") { testTails(_.tails3) }
  describe("tails4") { testTails(_.tails4) }
  describe("tails") { testTails(_.tails) }

  describe("scanRight") {
    it("should scan properly for finite lists") {
      Stream.empty[Int].scanRight(0)(_ + _).toList shouldBe List(0)
      Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)
    }

    describe("Stream.tails using scanRight") {
      def tails[A](s: Stream[A]): Stream[Stream[A]] =
        s.scanRight(Stream.empty[A]) { Stream.cons(_, _) }

      testTails(tails)
    }
  }
}
