package u03

import org.junit.*
import org.junit.Assert.*
import u03.Task2.*
import u03.Sequences.*
import u03.Sequences.Sequence.*
import u03.Streams.*
import u03.Streams.Stream.*

class Exercise2Test:
  import Sequences.*
  import Sequence.*
  import u02.Modules.Person.*

  @Test def testGetCourses(): Unit =
    val list = Cons(Teacher("pippo", "a"), Cons(Student("paolo", 2023), Cons(Teacher("pluto", "b"), Nil())))
    assertEquals(Cons("a", Cons("b", Nil())), getCoursesOfTeacher(list))

  @Test def testFoldLeft(): Unit =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft[Int](lst)(0)(_ - _))
    assertEquals(20, foldLeft[Int](lst)(4)(_ + _))
    assertEquals(210, foldLeft[Int](lst)(2)(_ * _))

  @Test def testNumberOfCourses(): Unit =
    val list = Cons(Teacher("pippo", "abc"), Cons(Student("paolo", 2023), Cons(Teacher("pluto", "b"), Cons(Teacher("paolo", "2023"),Nil()))))
    assertEquals(3, getNumberOfCourses(list))
    val list1 = Cons(Student("paolo", 2023), Nil())
    assertEquals(0, getNumberOfCourses(list1))

class Exercise3Test:
  @Test def testFill(): Unit =
    assertEquals(Stream.toList(Stream.fill(3)("a")), Cons("a", Cons("a", Cons("a", Nil()))))

  @Test def testFibo(): Unit =
    val f = Stream.toList(Stream.take(fibonacci)(5))
    assertEquals(f, Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil()))))))

  @Test def testCycle(): Unit =
    val repeat = cycle(Cons("a", Cons("b", Cons("c", Nil()))))
    assertEquals(Stream.toList(Stream.take(repeat)(5)),
      Cons("a", Cons("b", Cons("c", Cons("a", Cons("b", Nil()))))))