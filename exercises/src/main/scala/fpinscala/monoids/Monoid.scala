package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2
    override def zero: A => A = identity
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.fold(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as map f, m)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def flipped[A,B,C](f: (A, B) => C): (B, A) => C = (b, a) => f(a, b)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(flipped(f).curried)(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    def _foldMapV(as: IndexedSeq[A]): B =
      if (as.isEmpty) m.zero
      else if (as.size == 1) f(as.head)
      else m.op(_foldMapV(as.take(as.size / 2)), _foldMapV(as.drop(as.size / 2)))
    _foldMapV(as)
  }

  sealed abstract class Ordered
  case object Zero extends Ordered
  case object NotOrdered extends Ordered
  case class IsOrdered(left: Int, right: Int) extends Ordered

  val orderedIntMonoid = new Monoid[Ordered] {
    override def op(a1: Ordered, a2: Ordered): Ordered = (a1, a2) match {
      case (Zero, x) => x
      case (x, Zero) => x
      case (IsOrdered(a, b), IsOrdered(c, d))
          if b <= c => IsOrdered(a, d)
      case _ => NotOrdered
    }
    override def zero: Ordered = Zero
  }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, orderedIntMonoid) { n => IsOrdered(n, n) } != NotOrdered

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] =
      Par.flatMap(a1) { _a1 =>
        Par.map(a2) { _a2 =>
          m.op(_a1, _a2)
        }
      }

    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(a => Par.unit(f(a)))

  lazy val wcMonoid: Monoid[WC] = sys.error("todo")

  def count(s: String): Int = sys.error("todo")

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(m1: (A, B), m2: (A, B)): (A, B) = (A.op(m1._1, m2._1), B.op(m1._2, m2._2))
    val zero: (A, B) = (A.zero, B.zero)
  }


  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(f1: A => B, f2: A => B): A => B = a => B.op(f1(a), f2(a))
    def zero: A => B = _ => B.zero
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map.empty[K, V]
    def op(a: Map[K, V], b: Map[K, V]) = (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
      acc + (k -> V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
    }
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldLeft(as)(z)(flipped(f))

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldRight(as)(z)(flipped(f))

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldMap(as)(List(_))(listMonoid)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) =>
        val z_ = foldLeft(l)(z)(f)
        foldLeft(r)(z_)(f)
    }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    as match {
      case Leaf(a) => f(a, z)
      case Branch(l ,r) =>
        val z_ = foldRight(r)(z)(f)
        foldRight(l)(z_)(f)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as.fold(mb.zero)(a => f(a))

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as.fold(z)(f(z, _))

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as.fold(z)(f(_, z))
}
