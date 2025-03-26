package tasks

import u03.Sequences.*
import Sequence.*

object Tasks:
  //--------------------- TASK_1 (lists) ---------------------
  // Mandatory methods
  def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
    case _ if n <= 0 => s // se n è negativo o nullo, restitisce la sequenza originale
    case Cons(_, tail) => skip(tail)(n - 1) // skippa il primo elemento e decrementa n
    case Nil() => Nil() // se la sequenza è vuota, restituisce una sequenza vuota

  def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
    case (Cons(head1, tail1), Cons(head2, tail2)) => Cons((head1, head2), zip(tail1, tail2)) // Combina i primi elementi e procede ricorsivamente
    case _ => Nil() // se una delle due sequenze è vuota, viene restituita una sequenza vuota

  def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
    case Cons(head1, tail1) => Cons(head1, concat(tail1, s2)) // Si aggiunge il primo elemento della prima sequenza e si procede con la coda
    case Nil() => s2

  def reverse[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(head, tail) => concat(reverse(tail), Cons(head, Nil()))
    case Nil() => Nil()

  def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
    case Cons(head, tail) => concat(mapper(head), flatMap(tail)(mapper))
    case Nil() => Nil()
    
  // Optional methods
  import u03.Optionals.*
  import Optional.*
  
  def min(s: Sequence[Int]): Optional[Int] = s match
    case Nil() => Empty()
    case Cons(h, t) =>
      val tailMin = min(t) // chiamata ricorsiva sulla sequenza, fino ad arrivare a Nil()
      tailMin match
        case Empty() => Just(h)
        case Just(tailValue) => Just(if h < tailValue then h else tailValue)

  //--------------------- TASK_2 (more on lists) ---------------------
  import u02.Modules.*
  import Person.*

  def isTeacher(p: Person): Boolean = p match
    case Teacher(_, _) => true
    case _ => false

  def getCourse(p: Person): String = p match
    case Teacher(_, course) => course
    case _ => ""

  def teacherCourses(s: Sequence[Person]): Sequence[String] =
    map(filter(s)(isTeacher))(getCourse)






