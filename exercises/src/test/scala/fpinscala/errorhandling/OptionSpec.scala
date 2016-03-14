package fpinscala.errorhandling

import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import Function.const
import Option._
import org.scalatest._

class OptionSpec extends FunSpec with Matchers {

  def none[A]: Option[A] = None

  describe("map") {
    it("should map none to none") {
      None map identity shouldBe None
    }
    it("should map some to some") {
      Some(1) map identity shouldBe Some(1)
      Some(1) map { _ + 1 } shouldBe Some(2)
    }
  }

  describe("getOrElse") {
    it("should not use the default for some") {
      Some(1) getOrElse 2 shouldBe 1
    }
    it("should use the default for none") {
      None getOrElse 1 shouldBe 1
    }
  }

  describe("flatMap") {
    describe("Some") {
      it("should flatmap to something if the function returns some") {
        Some(1) flatMap Some.apply shouldBe Some(1)
        Some(1) flatMap { i => Some(i + 1) } shouldBe Some(2)
      }
      it("should flatmap to none if the function returns none") {
        Some(1) flatMap const(None) shouldBe None
      }
    }
    describe("None") {
      it("should always flatmap to None") {
        None flatMap Some.apply shouldBe None
        None flatMap const(None) shouldBe None
      }
    }
  }

  describe("orElse") {
    it("should ignore the default when present") {
      Some(1) orElse Some(2) shouldBe Some(1)
      Some(1) orElse None shouldBe Some(1)
    }
    it("should use the default when absent (the default can be none absent too)") {
      None orElse Some(2) shouldBe Some(2)
      None orElse None shouldBe None
    }
  }

  describe("filter") {
    it("should work with trivial predicates") {
      Some(1) filter const(true) shouldBe Some(1)
      Some(1) filter const(false) shouldBe None
    }
    it("should work with distinguishing predicates") {
      Some(1) filter { _ % 2 == 0 } shouldBe None
      Some(1) filter { _ % 2 == 1 } shouldBe Some(1)
    }
    it("should always be none if the option is none") {
      None filter const(true) shouldBe None
      None filter const(false) shouldBe None
    }
  }

  describe("map2") {
    it("should use both somes") {
      map2(Some(1), Some(2)) { _ + _ } shouldBe Some(3)
    }

    it("should use none if either are none") {
      map2(none[Int], Some(1)) { _ + _ } shouldBe None
      map2(Some(1), none[Int]) { _ + _ } shouldBe None
    }
  }

  describe("sequence") {
    it("should sequence to Some(empty) in the empty case") {
      sequence(List.empty) shouldBe Some(List.empty)
    }
    it("should sequence to Some(list) in the happy case") {
      sequence(List(Some(1), Some(2))) shouldBe Some(List(1, 2))
    }
    it("should sequence to None in the unhappy case") {
      sequence(List(None)) shouldBe None
      sequence(List(Some(1), None)) shouldBe None
    }
  }

  describe("traverse") {
    it("should traverse to Some(empty) in the empty case") {
      traverse(List.empty)(const(None)) shouldBe Some(List.empty)
      traverse(List.empty)(Some.apply) shouldBe Some(List.empty)
    }
    it("should traverse to Some(list) in the happy case") {
      traverse(List(1, 2))(Some.apply) shouldBe Some(List(1, 2))
    }
    it("should traverse to None in the unhappy case") {
      traverse(List(1, 2))(const(None)) shouldBe None
      traverse(List(None, Some(1)))(identity) shouldBe None
      traverse(List(Some(1), None))(identity) shouldBe None
    }
  }
}
