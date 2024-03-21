package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match
    case (n2: Int, rng2: RNG) if n2 == Int.MinValue => (Int.MaxValue, rng2)
    case (n2: Int, rng2: RNG) if n2 < 0 => (-n2, rng2)
    case (n2: Int, rng2: RNG) => (n2, rng2)
  

  def double(rng: RNG): (Double, RNG) =
    val (n2, rng2) = nonNegativeInt(rng)
    (n2.toDouble / Int.MaxValue.toDouble, rng2)

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (intVal, rng2) = rng.nextInt
    val (doubleVal, rng3) = double(rng2)
    ((intVal, doubleVal), rng3)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val ((intVal, doubleVal), rng2) = intDouble(rng)
    ((doubleVal, intVal), rng2)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (doubleVal1, rng2) = double(rng)
    val (doubleVal2, rng3) = double(rng2)
    val (doubleVal3, rng4) = double(rng3)
    ((doubleVal1, doubleVal2, doubleVal3), rng4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    def rec(n: Int, r: RNG, l: List[Int]): (List[Int], RNG) =
      if n <= 0 then
        (l, r)
      else
        val (nextVal, nextRng) = r.nextInt
        rec(n - 1, nextRng, nextVal :: l)
    rec(count, rng, List.empty)

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(n => n.toDouble / Int.MaxValue.toDouble)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng =>
    val (aVal, ra2) = ra(rng)
    val (bVal, rb2) = rb(ra2)
    (f(aVal, bVal), rb2)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rng =>
      def rec[A](rngs: List[Rand[A]], r: RNG, l: List[A]): (List[A], RNG) = rngs match
        case Nil => (l, r)
        case head :: next =>
          val (value, r2) = head(r)
          rec(next, r2, value :: l)
      rec(rs, rng, List.empty)

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = ???

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = ???

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      ???

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      ???

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      ???

  def apply[S, A](f: S => (A, S)): State[S, A] = f

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
