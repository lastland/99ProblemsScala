package Solver

object Solver {
  // Problem 1
  def last[A](lst: List[A]): A = lst match {
    case h :: Nil => h
    case _ :: t => last(t)
    case _ => throw new NoSuchElementException
  }

  // Problem 2
  def penultimate[A](lst: List[A]): A = lst match { 
    case h :: _ :: Nil => h
    case _ :: Nil => throw new NoSuchElementException
    case Nil => throw new NoSuchElementException
    case _ :: t => penultimate(t)
  }

  // Problem 3
  def nth[A](n: Int, lst: List[A]): A = (n, lst) match { 
    case (_, Nil) => throw new NoSuchElementException
    case (0, h :: _) => h
    case (n, _ :: t) if n < 0 => throw new NoSuchElementException
    case (_, _ :: t) => nth(n - 1, t)
  }
}
