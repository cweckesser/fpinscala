import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}
import scala.annotation.tailrec

// Exercise 7.1

/*
 def map2[A, B, C](pa: Par[A], pb: Par[B], f: (a: A, b: B) => C): Par[C]
*/

// Exercise 7.2, 7.3

// Can't use these in other exercises. Callable needs to be instantiated and we might as well import ExecutorService and Future too...
// trait ExecutorService:
//   def submit[A](c: Callable[A]): Future[A]

// trait Callable[A]:
//   def call[A]: A

// trait Future[A]:
//   def	cancel(mayInterruptIfRunning: Boolean): Boolean

//   def get: A

//   def isCancelled: Boolean

//   def isDone: Boolean

opaque type Par[A] = ExecutorService => Future[A]

// Copied from listing 7.4
object Par:
  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  private case class UnitFuture[A](value: A) extends Future[A]:
    def isDone = true
    def get = value
    def get(timeout: Long, units: TimeUnit) = value
    def isCancelled = false

  extension[A](pa:Par[A])
    def map2[B, C](pb: Par[B])(f: (A,B) => C): Par[C] =
        (es: ExecutorService) =>
          val futureA = pa(es)
          val futureB = pb(es)
          UnitFuture(f(futureA.get, futureB.get))

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  extension [A](pa: Par[A])
    def run(es: ExecutorService): Future[A] = pa(es)

  // Exercise 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = (a) => lazyUnit(f(a))

  // Exercise 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match
    case Nil => unit(Nil)
    case head :: next => head.map2(sequence(next))(_ :: _)

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    fork({
      val fbs: List[Par[B]] = as.map(asyncF(f))
      sequence(fbs)
    })

  // Exercise 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    lazyUnit(as.filter(f))

  // Exercise 7.7
  /*
    y.map(g).map(f) == y.map(f compose g)
    ???
  */
