package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(f andThen unit)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng1) = rng.nextInt
    val nonNegI =
      if (i == Int.MinValue) 0 // -MinValue > MaxValue, so map to 0 which is under-represented
      else if (i < 0) -i
      else i
    (nonNegI, rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, rng1)
  }

  def doubleWithMap: Rand[Double] =
    map(nonNegativeInt) { _.toDouble / Int.MaxValue }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def intDoubleWithBoth: Rand[(Int, Double)] = both(int, double)

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rng1) = intDouble(rng)
    ((d, i), rng1)
  }

  def doubleIntWithBoth: Rand[(Double, Int)] = both(double, int)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (list, newRng) =
      (1 to count).foldLeft((List.empty[Int], rng)) { case ((list, rng), _) =>
        val (i, nextRng) = rng.nextInt
        (i :: list, nextRng)
      }
    (list.reverse, newRng)
  }

  def intsWithSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit(f(a, b))
      }
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit(List.empty[A])) { (randListSoFar, rand) =>
      map2(rand, randListSoFar)(_ :: _)
    }


  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = f(rng)
      val (b, rng2) = g(a)(rng1)
      (b, rng2)
    }

}


object RNGMain extends App {
  import RNG._

  val rng = Simple(1)
  println("nonNegativeInt", nonNegativeInt(rng)._1)
  println("double", double(rng)._1)
  println("doubleWithMap", doubleWithMap(rng)._1)
  println("intDouble", intDouble(rng)._1)
  println("intDoubleWithBoth", intDoubleWithBoth(rng)._1)
  println("doubleInt", doubleInt(rng)._1)
  println("doubleIntWithBoth", doubleIntWithBoth(rng)._1)
  println("ints", ints(10)(rng)._1)
  println("intsWithSequence", intsWithSequence(10)(rng)._1)
  println(("sequence", sequence(List(int, int, int))(rng)._1))
  println(("flatMap", flatMap(int)(unit)(rng)._1))
  println(("mapWithFlatMap", mapWithFlatMap(int)(identity)(rng)._1))
  println(("map2WithFlatMap", map2WithFlatMap(int, int)((_, _))(rng)._1))
}

case class State[S,+A](run: S => (A, S)) {

  import State._

  def map[B](f: A => B): State[S, B] =
    flatMap(f andThen unit)

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a =>
      sb flatMap { b =>
        unit(f(a, b))
      }
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s1) = run(s)
      f(a).run(s1)
    }

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  private[this] def nextMachine(machine: Machine, input: Input): Machine =
    (machine, input) match {
      case (m@Machine(_, 0, _), _) => m
      case (m@Machine(true, _, _), Turn) => m
      case (m@Machine(false, _, _), Coin) => m
      case (m@Machine(false, candies, coins), Turn) => Machine(true, candies - 1, coins)
      case (m@Machine(true, candies, coins), Coin) => Machine(false, candies, coins + 1)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State { machine =>
      val resultMachine = inputs.foldLeft(machine)(nextMachine)
      ((resultMachine.coins, resultMachine.candies), resultMachine)
    }

  def simulateMachineWithCombinators(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val modifications: List[State[Machine, Unit]] =
      inputs.map(input => modify[Machine](machine => nextMachine(machine, input)))
    for {
      _ <- sequence(modifications)
      m <- get
    } yield (m.coins, m.candies)
  }

  def unit[S,A](a: A): State[S,A] = State(s => (a, s))

  def sequence[S,A](ss: List[State[S,A]]): State[S, List[A]] =
    ss.reverse.foldLeft(unit[S,List[A]](List.empty[A])) { (states, state) =>
      state.map2(states)(_ :: _)
    }

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}


object StateMain extends App {
  import State._

  val rng: RNG = RNG.Simple(1)

  val int: Rand[Int] = State(_.nextInt)
  val double: Rand[Double] = int map { _.toDouble / Int.MaxValue }

  println(("int", int.run(rng)))
  println(("double", double.run(rng)))

  val prog1: Rand[(Int, Int, Double)] =
    for {
      i <- int
      d <- double
      _ <- set(rng)
      j <- int
    } yield (i,j,d)

  println("prog1", prog1.run(rng))

  val instructions = List(Coin, Turn, Coin, Turn)

  println("simulateMachine", simulateMachine(instructions).run(Machine(true, 10, 0)))
  println("simulateMachineWithCombinators",
    simulateMachineWithCombinators(instructions).run(Machine(true, 10, 0)))
}

