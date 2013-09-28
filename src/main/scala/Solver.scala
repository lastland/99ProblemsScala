package S99

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

  // Problem 4
  def length[A](lst: List[A]): Int = length(lst, 0)

  private def length[A](lst: List[A], res: Int): Int = lst match { 
    case Nil => res
    case _ :: t => length(t, res + 1)
  }

  // Problem 5
  def reverse[A](lst: List[A]): List[A] = reverse(lst, Nil)

  private def reverse[A](lst: List[A], res: List[A]): List[A] = lst match { 
    case Nil => res
    case h :: t => reverse(t, h :: res)
  }

  // Problem 6
  def isPalindrome[A](lst: List[A]): Boolean = lst == reverse(lst) // Yes. I'm lazy.

  // Problem 7
  def flatten(lst: List[Any]): List[Any] = lst flatMap { 
    case l: List[_] => flatten(l)
    case elt => List(elt)
  }

  // Problem 8
  def compress[A](lst: List[A]): List[A] = reverse(compress(lst, Nil))

  private def compress[A](lst: List[A], res: List[A]): List[A] = lst match {
    case x :: y :: t if x == y => compress(y :: t, res)
    case x :: y :: t if x != y => compress(y :: t, x :: res)
    case x :: Nil => x :: res
    case _ => res
  }

  // Problem 9
  def pack[A](lst: List[A]): List[List[A]] = reverse(pack(lst, Nil, Nil))

  private def pack[A](lst: List[A], now: List[A], res: List[List[A]]): List[List[A]] = lst match { 
    case x :: y :: t if x == y => pack(y :: t, x :: now, res)
    case x :: y :: t if x != y => pack(y :: t, Nil, (x :: now) :: res)
    case x :: Nil => (x :: now) :: res
    case _ => res
  }

  // Problem 10
  def encode[A](lst: List[A]): List[(Int, A)] = encode2(pack(lst))

  private def encode2[A](lst: List[List[A]]): List[(Int, A)] = lst map { 
    l => (l.length, l.head)
  }

  // Problem 11
  def encodeModified[A](lst: List[A]): List[Any] = encodeModified2(pack(lst))

  def encodeModified2[A](lst: List[List[A]]): List[Any] = lst map { 
    l => l.length match { 
      case 1 => l.head
      case _ => (l.length, l.head)
    }
  }

  // Problem 12
  def decode[A](lst: List[(Int, A)]): List[A] = lst flatMap { 
    e => repeat(e._1, e._2)
  }

  private def repeat[A](n: Int, elt: A): List[A] = repeat(n, elt, Nil)

  private def repeat[A](n: Int, elt: A, res: List[A]): List[A] = n match { 
    case 0 => res
    case n if n < 0 => throw new IllegalArgumentException
    case _ => repeat(n - 1, elt, elt :: res)
  }

  // Problem 13
  def encodeDirect[A](lst: List[A]): List[(Int, A)] = 
    if (lst == Nil) Nil
    else { 
      val (f, s) = lst.span(_ == lst.head)
      (f.length, f.head) :: encodeDirect(s)
    }

  // Problem 14
  def duplicate[A](lst: List[A]): List[A] = lst flatMap { 
    e => repeat(2, e)
  }

  // Problem 15
  def duplicateN[A](n: Int, lst: List[A]): List[A] = lst flatMap { 
    e => repeat(n, e)
  }

  // Problem 16
  def drop[A](n: Int, lst: List[A]): List[A] = reverse(drop(n, 1, lst, Nil))

  private def drop[A](n: Int, p: Int, lst: List[A], res: List[A]): List[A] = lst match
  {
    case Nil => res
    case h :: t => if (p == n) drop(n, 1, t, res)
	      else drop(n, p + 1, t, h :: res)
  }

  // Problem 17
  def split[A](n: Int, lst: List[A]): (List[A], List[A]) = split(n, lst, Nil)

  private def split[A](n: Int, lst: List[A], res: List[A]): (List[A], List[A]) =
    if (n == 0 || lst.isEmpty) (reverse(res), lst)
    else if (n > 0) split(n - 1, lst.tail, lst.head :: res)
    else split(lst.length + n, lst, Nil)

  // Problem 18
  def slice[A](i: Int, k: Int, lst: List[A]): List[A] = reverse(slice(i, k, 0, lst, Nil))
  
  private def slice[A](i: Int, k: Int, n: Int, lst: List[A], res: List[A]): List[A] =
    if (lst.isEmpty) res
    else if (n < i) slice(i, k, n + 1, lst.tail, Nil)
    else if (n >= i && n < k) slice(i, k, n + 1, lst.tail, lst.head :: res)
    else res

  // Problem 19
  def rotate[A](n: Int, lst: List[A]): List[A] = { 
    val t = split(n, lst)
    t._2 ::: t._1
  }
}
