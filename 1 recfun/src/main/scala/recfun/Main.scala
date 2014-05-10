package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }
/*
 * My first solution started at the top of the triangle
 * and recursively generated each row until the target row
 * then simply returned the target column from that row.
  def pascal2(c: Int, r: Int): Int = {
    if (c < 0 || r < 0) throw new NoSuchElementException(s"Column ($c), and row ($r) cannot be negative.")
    if (c > r) throw new NoSuchElementException(s"No column $c in row $r")
    
    // given a row from pascal's triangle, produce the next row
    def nextRow(curRow: Array[Int]): Array[Int] = {
      val nxt = new Array[Int](curRow.length + 1)
      for (i <- nxt.indices) {
        nxt(i) = curRow.slice(i - 1, i + 1).sum
      }
      nxt
    }

    def pascalIter(c: Int, r: Int, row: Array[Int]): Int = {
      if (r == row.length-1) row(c) // found the row, return the col
      else pascalIter(c, r, nextRow(row)) // not there yet, get the next row
    }
    
    pascalIter(c, r, Array(1))
  }
*/
  /**
   * Exercise 1
   * 
   * My final implementation starts at the target node and walks up the triangle. 
   * 
   * If the target node is on an edge (col==0 or col==row) then its value is simply 1.
   * 
   * If the target node is an interior node, sum the number of edge nodes reachable
   * from the left node above it with the number reachable from the right node above
   * it. Because of the recursive nature, an edge node may be counted multiple times
   * if it is reachable by more than one antecedent interior node.
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || c > r) 0 // out of bounds
    else if (c == 0) 1 // left edge node
    else if (c == r) 1 // right edge node
    else pascal(c-1, r-1) + pascal(c, r-1) // interior node
  }

  /**
   * Exercise 2
   * 
   * First filter out non-parentheses characters, then process open parens and close parens
   * keeping track of the number of unmatched opens.
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(unmatchedOpens: Int, tail: List[Char]): Boolean = {
      if (tail.isEmpty) unmatchedOpens == 0
      else {
        if (tail.head == '(') balanceIter(unmatchedOpens+1, tail.tail) // open
        else unmatchedOpens > 0 && balanceIter(unmatchedOpens-1, tail.tail)   // close
      }
    }
    balanceIter(0,chars.filter({List('(',')').contains(_)}))
  }
  
  /**
   * Exercise 3
   * 
   * For each coin, split the solutions into those that include the coin and those
   * the don't. 
   * 
   * To find solutions that include the coin, subtract its value from the
   * amount and recursively continue with the same set of coins (so the same coin may be
   * counted more than once). 
   * 
   * To find solutions that don't include the coin, remove it from the list and recursively
   * continue with the same amount of money, but with only the remaining coins.
   * 
   * When a sum is reached that is greater than the target amount, remaining amount will be < 0, 
   * so don't count it as a valid solution (that is, return 0).
   * 
   * When the remaining amount is == 0, then an exact sum is reached, increment the solution count (return 1).
   */
  def countChange(amount: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else if (amount < 0) 0
    else if (amount == 0) 1
    else {
      val usingHead = countChange(amount-coins.head, coins)
      val notUsingHead = countChange(amount, coins.tail)
      usingHead + notUsingHead
    }
  }
}
