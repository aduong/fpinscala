package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def toList: List[A] = this.foldRight(List.empty[A]) { _ :: _ }

  def isEmpty: Boolean = this == Empty

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def find(f: A => Boolean): Option[A] = find1(f)

  @annotation.tailrec
  final def find1(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find1(f)
  }

  def find2(f: A => Boolean): Option[A] = (this filter f).headOption

  def take(n: Int): Stream[A] = take1(n)

  def take1(n: Int): Stream[A] = (n, this) match {
    case (0, _) | (_, Empty) => Empty
    case (n, Cons(h, t)) => Cons(h, () => t().take(n - 1))
  }

  def take2(n: Int): Stream[A] =
    this zip Stream.from(0) takeWhile {
      case (x, i) => i < n
    } map {
      case (x, _) => x
    }

  def take3(n: Int): Stream[A] = Stream.unfold((n, this)) {
    case (0, _) => None
    case (_, Empty) => None
    case (n, Cons(h, t)) => Some((h(), (n - 1, t())))
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = (n, this) match {
    case (_, Empty) => Empty
    case (0, s) => s
    case (n, Cons(h, t)) => t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = takeWhile1(p)

  def takeWhile1(p: A => Boolean): Stream[A] = this.foldRight(empty[A]) {
    (a, taken) => if (p(a)) cons(a, taken) else empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = zipWith1(s)(f)

  def zipWith1[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = (this, s) match {
    case (Cons(h1, t1), Cons(h2, t2)) => cons(f(h1(), h2()), t1().zipWith(t2())(f))
    case _ => Empty
  }

  def zipWith2[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zip[B](s: Stream[B]): Stream[(A, B)] = this.zipWith(s) { (_, _) }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = zipAll1(s)

  def zipAll1[B](s: Stream[B]): Stream[(Option[A], Option[B])] = (this, s) match {
    case (Cons(h1, t1), Cons(h2, t2)) => cons((Some(h1()), Some(h2())), t1() zipAll1 t2())
    case (Cons(h1, t1), Empty) => cons((Some(h1()), None), t1() zipAll1 Empty)
    case (Empty, Cons(h2, t2)) => cons((None, Some(h2())), Empty zipAll1 t2())
    case (Empty, Empty) => Empty
  }

  def deconstruct[C](c: Stream[C]): (Option[C], Stream[C]) = c match {
    case Cons(h, t) => (Some(h()), t())
    case _ => (None, Empty)
  }

  def zipAll2[B](s: Stream[B]): Stream[(Option[A], Option[B])] = {
    (deconstruct(this), deconstruct(s)) match {
      case ((None, _), (None, _)) => Empty
      case ((h1, t1), (h2, t2)) => cons((h1, h2), t1 zipAll1 t2)
    }
  }

  def zipAll3[B](s: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, s)) { case (s1, s2) =>
    (deconstruct(s1), deconstruct(s2)) match {
      case ((None, _), (None, _)) => None
      case ((h1, t1), (h2, t2)) => Some((h1, h2), (t1, t2))
    }
  }


  def forAll(p: A => Boolean): Boolean = !this.exists(a => !p(a)) // de morgan's ;)

  def headOption: Option[A] = headOption1

  // the reasonable way to implement headOption...
  def headOption1: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // the unreasonable, clever way to implement headOption
  def headOption2: Option[A] = this.foldRight(None: Option[A]) { (a, _) => Some(a) }


  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = startsWith1(s)

  def startsWith1[B](s: Stream[B]): Boolean = (this zipAll s) takeWhile {
    case (_, bo) => bo.isDefined
  } forAll {
    case (Some(a), Some(b)) if a == b => true
    case _ => false
  }

  def map[B](f: A => B): Stream[B] = map1(f)

  def map1[B](f: A => B): Stream[B] = this match {
    case Empty => Empty
    case Cons(h, t) => cons(f(h()), t().map1(f))
  }

  def map2[B](f: A => B): Stream[B] = this.foldRight(empty[B]) {
    (a, res) => cons(f(a), res)
  }

  def map3[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def filter(p: A => Boolean): Stream[A] = this.foldRight(empty[A]) { (a, as) =>
    if (p(a)) cons(a, as)
    else as
  }

  def append[B>:A](s: => Stream[B]): Stream[B] = this.foldRight(s) { cons(_, _) }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    (this map f).foldRight(empty[B])(_ append _)

  def tails: Stream[Stream[A]] = tails1

  def tails1: Stream[Stream[A]] = cons(this, Stream.unfold(this) {
    case Empty => None
    case Cons(_, t) => Some(t(), t())
  })

  def tails2: Stream[Stream[A]] = this match {
    case Empty => Stream(Empty)
    case Cons(_, t) => cons(this, t().tails)
  }

  def tails3: Stream[Stream[A]] = this.foldRight(Stream(Stream.empty[A])) { (x, acc) =>
    cons(cons(x, acc.headOption.get), acc) // acc.headOption is always present
  }

  def tails4: Stream[Stream[A]] =
    this.scanRight(Stream.empty[A]) { cons(_, _) }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this.foldRight(Stream(z)) { (a, bs) =>
    cons(f(a, bs.headOption.get), bs) // bs.headOption is always present
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant1[A](a: A): Stream[A] = cons(a, constant(a))
  def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))
  def constant[A](a: A): Stream[A] = constant1(a)

  def from1(n: Int): Stream[Int] = cons(n, from(n + 1))
  def from2(n: Int): Stream[Int] = unfold(0)(i => Some(i, i + 1))
  def from(n: Int): Stream[Int] = from1(n)

  val fibs1: Stream[Int] = unfold((0, 1)) { case (i, j) => Some((i, (j, i + j))) }
  val fibs2: Stream[Int] = cons(0, cons(1, (fibs2 zipWith (fibs2 drop 1))(_ + _)))
  val fibs: Stream[Int] = fibs1

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
}
