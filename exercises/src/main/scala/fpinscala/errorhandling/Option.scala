package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import scala.annotation.tailrec

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this map f getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] = this map { Some(_) } getOrElse ob

  def filter(f: A => Boolean): Option[A] = this flatMap { a =>
    if (f(a)) Some(a)
    else None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] =
    for {
      m <- mean(xs)
      sos = xs.map(x => math.pow(x - m, 2)).sum
      n = xs.length
    } yield sos / n

  def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- oa
      b <- ob
    } yield f(a, b)

  def concat[A](ao: Option[List[A]], bo: Option[List[A]]): Option[List[A]] =
    for {
      a <- ao
      b <- bo
    } yield a ++ b

  def sequence[A](aos: List[Option[A]]): Option[List[A]] =
    aos.map(_.map(List(_))) // lift List[Option[A]] to List[Option[List[A]]]
      .fold(Some(List.empty))(concat)


  @tailrec
  def _traverse1[A,B](aos: List[A], acc: List[B] = Nil)(f: A => Option[B]): Option[List[B]] =
    if (aos.isEmpty) Some(acc)
    else f(aos.head) match {
      case Some(b) => _traverse1(aos.tail, b :: acc)(f)
      case None => None
    }

  def traverse1[A, B](aos: List[A])(f: A => Option[B]): Option[List[B]] =
    _traverse1(aos)(f).map(_.reverse)

  def traverse2[A,B](aos: List[A])(f: A => Option[B]): Option[List[B]] =
    aos match {
      case ao :: rest => map2(f(ao), traverse2(rest)(f)) { _ :: _ }
      case _ => Some(List.empty)
    }

  def traverse3[A,B](aos: List[A])(f: A => Option[B]): Option[List[B]] =
    aos.foldRight(Some(List.empty): Option[List[B]]) { (ao, acc) => map2(f(ao), acc)(_ :: _) }


  def traverse[A, B](aos: List[A])(f: A => Option[B]): Option[List[B]] = traverse3(aos)(f)
}
