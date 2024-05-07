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
    case (n2: Int, rng2: RNG) if n2 < 0 => (-n2 + 1, rng2)
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

  // doubleViaMap
  def _double: Rand[Double] =
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

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rng2) = r(rng)
      f(a)(rng2)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = r(rng)
      (f(a), rng2)

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] = s =>
      val (a, s2) = run(s)
      (f(a), s2)

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = sa =>
      val (a, sa2) = run(sa)
      val (b, sb2) = sb(sa2)
      (f(a, b), sb2)

    def flatMap[B](f: A => State[S, B]): State[S, B] = s =>
      val (a, s2) = run(s)
      f(a)(s2)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] = initialState =>
    def rec[A](remainingStates: List[State[S, A]], nextState: S, l: List[A]): (List[A], S) = remainingStates match
      case Nil => (l.reverse, nextState) // <- Reversing list here, because the test assumes the usage of foldRight
      case head :: tail =>
        val (value, nextState2) = head(nextState)
        rec(tail, nextState2, value :: l)
    rec(states, initialState, List.empty)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      // The following is the "traverse" implementation. In this case, since the "modify" function does not return a produced value
      // because it only modifies the underlying state, so the list of "traversed" values is disregarded (by keeping a list of "Unit")
      _ <- inputs.foldRight(State.unit[Machine, List[Unit]](Nil))((i, acc) => State.modify(processInput(i)).map2(acc)(_ :: _))
      s <- State.get
    } yield ((s.coins, s.candies))

  val processInput = (i: Input) => (m: Machine) => (i, m) match
    case (_, Machine(_, 0, _)) => m
    case (Input.Coin, Machine(false, _, _)) => m
    case (Input.Turn, Machine(true, _, _)) => m
    case (Input.Coin, Machine(true, candies, coins)) => Machine(false, candies, coins +1)
    case (Input.Turn, Machine(false, candies, coins)) => Machine(true, candies -1, coins)
