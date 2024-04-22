package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter
import scala.{Option as _, Some as _, None as _}
import scala.annotation.tailrec

enum Option[+A]:
  case Some(get: A)
  case None

  // Usage: transforms the encapsulated value
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }

  // Usage: retrieves the encapsulated value or a default one if the first is not defined
  def getOrElse[B>:A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }

  // Usage: transforms the encapsulated value without deeply-nesting the containerized value
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(value) => f(value)
    case None => None
  }

  // Usage: retrieves the encapsulated value within its container or a default value, containerized
  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None => ob
  }

  // Usage: determine whether the encapsulated value meets a condition
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(value) => if f(value) then this else None
    case None => None
  }

  // flatMap via map and getOrElse
  def flatMap2[B](f: A => Option[B]): Option[B]  = map(f).getOrElse(None)

  // orElse via map and getOrElse
  def orElse2[B>:A](ob: => Option[B]): Option[B] = map(a => Some(a)).getOrElse(ob)

  // filter via map and getOrElse
  def filter2(f: A => Boolean): Option[A] = if map(f).getOrElse(false) then this else None

object Option:

  def failingFn(i: Int): Int =
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch case e: Exception => 43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    val meanValue = mean(xs)

    val variance = meanValue.flatMap(m => {
      val squaredSpreads = xs.map(x => Math.pow(x - m, 2))
      mean(squaredSpreads)
    })

    variance
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (someA: Some[A], someB: Some[B]) => Some(f(someA.get, someB.get))
    case _ => None
  }

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil => Some(Nil)
    case head :: next => head.flatMap((headValue) => sequence(next).map(headValue :: _))
  }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case Nil => Some(Nil)
    case head :: next => f(head).flatMap(fHead => traverse(next)(f).map(fHead :: _))
  }
