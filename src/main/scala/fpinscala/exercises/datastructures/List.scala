package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match
    case Nil => sys.error("There cannot be a tail on an empty list.")
    case Cons(_, t) => t

  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil => sys.error("Cannot set the head of an empty list.")
    case Cons(_, t) => Cons(h, t)

  def drop[A](l: List[A], n: Int): List[A] =
    if n <= 0 then l
    else
      l match
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = l match
    case Nil => sys.error("Cannot drop last element for an empty list.")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil => acc
    case Cons(h, t) => foldLeft(t, f(acc, h), f)

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A](), (acc, x) => Cons(x, acc))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r, Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = foldLeft(
    l,
    List[A](),
    (acc, listA) => listA match
      case Nil => acc
      case cons @ Cons(_, _) => append(acc, cons)
  )

  def incrementEach(l: List[Int]): List[Int] = foldLeft(
    l,
    List[Int](),
    (acc, integer) => append(acc, Cons(integer + 1, Nil))
  )

  def doubleToString(l: List[Double]): List[String] = foldLeft(
    l,
    List[String](),
    (acc, double) => append(acc, Cons(double.toString(), Nil))
  )

  def map[A,B](l: List[A], f: A => B): List[B] = foldLeft(
    l,
    List[B](),
    (acc, a) => append(acc, Cons(f(a), Nil))
  )

  def filter[A](as: List[A], f: A => Boolean): List[A] = foldLeft(
    as,
    List[A](),
    (acc, a) =>
      if f(a) then append(acc, Cons(a, Nil))
      else acc
  )

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] = foldLeft(
    as,
    List[B](),
    (acc, a) => append(acc, f(a))
  )

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, (a) => if f(a) then Cons(a, Nil) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(aHead, aTail), Cons(bHead, bTail)) => Cons(aHead + bHead, addPairwise(aTail, bTail))
  }

  def addPairwise2(a: List[Int], b: List[Int]): List[Int] = addPairwiseGeneric(a, b, _ + _)

  def addPairwiseGeneric[A, B](a1: List[A], a2: List[A], f: (A, A) => B): List[B] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a1Head, a1Tail), Cons(a2Head, a2Tail)) =>
      Cons(f(a1Head, a2Head), addPairwiseGeneric(a1Tail, a2Tail, f))
  }

  // def zipWith - TODO determine signature

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Cons(supH, supT), Cons(subH, subT)) =>
      if supH == subH then hasSubsequence(supT, subT)
      else hasSubsequence(supT, sub)
    case (Nil, Cons(_, _)) => false
    case (_, _) => true
  }

  // @annotation.tailrec
  def foldRightViaFoldLeft[A,B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(as, acc, (b: B, a: A) => f(a, b))

  // @annotation.tailrec
  def foldLeftViaFoldRight[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    foldRight(l, acc, (a: A, b: B) => f(b, a))
