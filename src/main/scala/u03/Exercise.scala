package u03

import u02.Modules.Person

import scala.annotation.tailrec

object Exercise:
  import Sequences.*
  import Sequence.*
  import u02.Modules.Person.*

  def getCoursesOfTeacher(s: Sequence[Person]) : Sequence[String] = flatMap(s)(v => v match
    case Teacher(_, c) => Cons(c, Nil())
    case _ => Nil())

  @tailrec
  def foldLeft[A](s: Sequence[A])(n: A)(fun: (A, A) => A): A = s match
    case Cons(h, t) => foldLeft(t)(fun(n,h))(fun)
    case _ => n

  //first filter for Teacher, then map each teacher to the length of their
  //courses list, and finally apply foldLeft to sum up these counts.
  def getNumberOfCourses(s: Sequence[Person]): Int =
    foldLeft(map(filter(s)(Teacher => true)) {// le graffe le suggerisce di metterle intellij
      case Teacher(_, c) => 1
      case _ => 0
    })(0)(_ + _)