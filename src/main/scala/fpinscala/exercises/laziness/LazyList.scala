package fpinscala.exercises.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => List.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1 => LazyList.cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => LazyList.cons(h(), Empty)
    case Cons(_, _) => Empty
    case Empty => Empty

  def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case current @ Cons(h, t) if n == 0 => current
    case Cons(_, _) => Empty
    case Empty => Empty

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => LazyList.cons(h(), t().takeWhile(p))
    case Cons(_, _) => Empty
    case Empty => Empty

  def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty)(
      (h, t) =>
        if p(h) then LazyList.cons(h, t) else Empty
    )

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    foldRight(LazyList.empty)((h, t) => LazyList.cons(f(h), t))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty)(
      (h, t) =>
        if f(h) then LazyList.cons(h, t) else t
    )

  def append[AA >: A](l: => LazyList[AA]): LazyList[AA] = // Why is this "AA >: A" necessary?
    foldRight(l)((h, t) => LazyList.cons(h, t))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(LazyList.empty)((h, t) => f(h).append(t))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    LazyList.unfold(this)
      (
        a => a match
          case Cons(h, t) => Some(f(h()), t())
          case Empty => None
      )

  def takeViaUnfold(n: Int): LazyList[A] =
    LazyList.unfold((this, n))
      (
        (a, s) => a match
          case Cons(h, t) if s > 0 => Some(h(), (t(), s - 1))
          case _ => None
      )

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    LazyList.unfold(this)
      (
        (a) => a match
          case Cons(h, t) if p(h()) => Some(h(), t())
          case _ => None
      )

  def zipWith[B, C](b: LazyList[B])(f: (A, B) => C): LazyList[C] =
    LazyList.unfold((this, b))
      (
        (a, b) => (a, b) match
          case (Cons(hA, tA), Cons(hB, tB)) => Some(f(hA(), hB()), (tA(), tB()))
          case _ => None
      )

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    LazyList.unfold((this, that))
      (
        (a, b) => (a, b) match
          case (Cons(hA, tA), Cons(hB, tB)) => Some((Some(hA()), Some(hB())), (tA(), tB()))
          case (Cons(hA, tA), Empty) => Some((Some(hA()), None), (tA(), Empty))
          case (Empty, Cons(hB, tB)) => Some((None, Some(hB())), (Empty, tB()))
          case (Empty, Empty) => None
      )

  def hasSubsequence[A](that: LazyList[A]): Boolean = (this, that) match
    case (Cons(hThis, tThis), Cons(hThat, tThat)) =>
      if hThis() != hThat() then tThis().hasSubsequence(that)
      else tThis().hasSubsequence(tThat())
    case (Empty, Cons(hThat, tThat)) => false
    case (_, _) => true

  def startsWith[B](s: LazyList[B]): Boolean =
    (this.headOption, s.headOption) match
      case (Some(a), Some(b)) => a == b && this.hasSubsequence(s)
      case (_, None) => true
      case (_, _) => false

  def tails: LazyList[LazyList[A]] =
    val unfoldedList = LazyList.unfold[LazyList[A], LazyList[A]](this)
      (
        a => a match
          case Cons(h, t) => Some((Cons(h, t), t()))
          case Empty => None
      )
    unfoldedList.append(LazyList(Empty))

  /**
    * This implementation works but its time is not linear!
    *
    * @param acc
    * @param f
    * @return
    */
  def scanRight[B](acc: B)(f: (A, => B) => B): LazyList[B] =
    println("--- START ---")
    val res = foldRight[(B, LazyList[B])]((acc, LazyList(acc)))(
      (a, z) =>
        val fA = f(a, z._1)
        println(s"a -> $a / f(a) -> $fA / z._1: B -> ${z._1} / z._2 -> ${z._2.toList}")
        (fA, LazyList.cons(fA, z._2))
    )._2
    println(s" RESULT -> ${res.toList}")
    println("--- END ---")
    res
    

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    def auxFibs(n1: Int, n2: Int): LazyList[Int] =
      LazyList.cons(n1, auxFibs(n2, n1 + n2))
    val first = 0
    val second = 1
    auxFibs(first, second)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    val next = f(state)
    next match
      case Some(nextValue, nextState) => LazyList.cons(nextValue, unfold(nextState)(f))
      case None => Empty

  lazy val fibsViaUnfold: LazyList[Int] = unfold((0, 1))((a, s) => Some(a, (s, a + s)))

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(a => Some(a, a + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(a)(a => Some(a, a))

  lazy val onesViaUnfold: LazyList[Int] = continuallyViaUnfold(1)
