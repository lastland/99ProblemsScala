object Solver {
  def last[A](lst: List[A]): A = 
    if (lst.tail == Nil) lst.head
    else last(lst)
 
  def main(args: Array[String]) { 
    println("Hello!")
  }
}
