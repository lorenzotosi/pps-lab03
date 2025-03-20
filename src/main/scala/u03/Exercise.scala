package u03

import u02.Modules.Person
import u03.Optionals.Optional
import u03.Optionals.Optional.*
import u03.Streams.Stream.*

import scala.annotation.tailrec

object Exercise:
  import Sequences.*
  import Sequence.*
//TASK 1
  @tailrec
  def skip[A](s: Sequence[A])(n: Int): Sequence[A] = (s, n) match
    case (Cons(head, tail), n) if n > 0 => skip(tail)(n - 1)
    case _ => s

  def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
    case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
    case _ => Nil()

  def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
    case Cons(h1, t1) => Cons(h1, concat(t1, s2))
    case _ => s2

  def reverse[A](s: Sequence[A]): Sequence[A] =
    @tailrec
    def _reverse(acc: Sequence[A], curr: Sequence[A]): Sequence[A] = acc match
      case Nil() => curr
      case Cons(h, t) => _reverse(t, Cons(h, curr))
    _reverse(s, Nil())

  def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
    case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
    case _ => Nil()

  def min(s: Sequence[Int]): Optional[Int] = s match
    case Cons(h, t) => min(t) match
      case Just(m) if m < h => Just(m)
      case _ => Just(h)
    case Nil() => Empty()

  def minTail(s: Sequence[Int]): Optional[Int] =
    @tailrec
    def _min(seq: Sequence[Int], min: Int): Int = seq match
      case Cons(h, t) => _min(t, if (h < min) h else min)
      case Nil() => min
    s match
      case Cons(h, t) => Just(_min(t, h))
      case Nil() => Empty()

  def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h, t) => Cons(h, evenIndices(skip(t)(1)))
    case _ => Nil()

  @tailrec
  def contains[A](s: Sequence[A])(elem: A): Boolean = s match
    case Cons(h, t) => h.==(elem) || contains(t)(elem)
    case _ => false

  def distinct[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h, t) => Cons(h, distinct(filter(t)(_ != h)))
    case _ => Nil()

  def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
    (filter(s)(pred), filter(s)(!pred(_)))

//TASK 2
object Task2:
  import Sequences.*
  import Sequence.*
  import u02.Modules.Person.*
  def getCoursesOfTeacher(s: Sequence[Person]) : Sequence[String] = flatMap(s)(v => v match
    case Teacher(_, c) => Cons(c, Nil())
    case _ => Nil())

  @tailrec
  def foldLeft[A, B](s: Sequence[A])(n: B)(fun: (B, A) => B): B = s match
    case Cons(h, t) => foldLeft(t)(fun(n, h))(fun)
    case _ => n

  def getNumberOfCourses(s: Sequence[Person]): Int =
    foldLeft(map(getCoursesOfTeacher(s))(x => 1))(0)(_ + _)

  def getNumberOfCoursesVariant(s: Sequence[Person]): Int =
    foldLeft(map(filter(s) {
      case Teacher(_, _) => true
      case _ => false
    })(x => 1))(0)(_ + _)

//TASK 3
object Task3:
  import Sequences.*
  import Sequence.*
  import u03.Streams.*

  //l'implementazione di takeWhile e interleave era gia fornita.

  def fill[A](n: Int)(k: A): Stream[A] = n match
    case n if n > 0 => cons(k, fill(n - 1)(k))
    case _ => empty()

  val fibonacci: Stream[Int] =
    def fib(a: Int, b: Int): Stream[Int] =
      cons(a, fib(b, a + b))
    fib(0, 1)

  def cycle[A](lst: Sequence[A]): Stream[A] = lst match
    case Cons(h, t) => cons(h, cycle(concat(t, Cons(h, Nil()))))
    case _ => empty()
