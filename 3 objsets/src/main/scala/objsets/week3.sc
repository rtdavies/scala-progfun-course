package objsets

object week3 {
  trait List[T] {
    def head: T
    def tail: List[T]
    def +:(elem: T): List[T]
    def isEmpty: Boolean
    def size: Int
  }
  
  class Nope[T] extends List[T] {
  	def head: Nothing = throw new NoSuchElementException("Nil.head")
  	def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  	def +:(elem: T): List[T] = new Cons(elem, new Nope)
  	def isEmpty: Boolean = true
  	def size: Int = 0
  }
  
  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def +:(elem: T): List[T] = new Cons[T](elem, tail.+:(head))
    def isEmpty: Boolean = false
    def size: Int = 1+tail.size
  }
  
  def nth[T](n: Int, l: List[T]): T = {
    def step(curCount: Int, tail: List[T]): T = {
      if (curCount == n) tail.head
      else (step(curCount+1, tail.tail))
    }
    if (n < 0 || n >= l.size) throw new IndexOutOfBoundsException(s"$n")
    step(0, l)
  }                                               //> nth: [T](n: Int, l: objsets.week3.List[T])T
  nth(0, new Nope[Int].+:(10).+:(20).+:(30).+:(40).+:(50).+:(60))
                                                  //> res0: Int = 60
  
}