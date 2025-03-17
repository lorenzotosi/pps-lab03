package u03

import org.junit.*
import org.junit.Assert.*

class ExerciseTest:
  import Sequences.*
  import Sequence.*
  import u02.Modules.Person.*
  import Exercise.*

  @Test def testGetCourses(): Unit =
    val list = Cons(Teacher("pippo", "a"), Cons(Student("paolo", 2023), Cons(Teacher("pluto", "b"), Nil())))
    assertEquals(Cons("a", Cons("b", Nil())), getCoursesOfTeacher(list))

  @Test def testFoldLeft(): Unit =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft[Int](lst)(0)(_ - _))
    assertEquals(20, foldLeft[Int](lst)(4)(_ + _))
    assertEquals(210, foldLeft[Int](lst)(2)(_ * _))

  @Test def testNumberOfCourses(): Unit =
    val list = Cons(Teacher("pippo", "a"), Cons(Student("paolo", 2023), Cons(Teacher("pluto", "b"), Nil())))
    assertEquals(2, getNumberOfCourses(list))
    val list1 = Cons(Student("paolo", 2023), Nil())
    assertEquals(0, getNumberOfCourses(list1))