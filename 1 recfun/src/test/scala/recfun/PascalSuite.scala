package recfun

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.util.NoSuchElementException

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main._

// tests for my first implementation
//  test("col < 0 throws an exception") {
//    intercept[NoSuchElementException] {
//      pascal(-1,0)
//    }
//  }
//  
//  test("row < 0 throws an exception") {
//    intercept[NoSuchElementException] {
//      pascal(0,-1)
//    }
//  }
//  
//  test("col > row throws an exception") {
//    intercept[NoSuchElementException] {
//      pascal(1,0)
//    }
//  }
  
  test("col < 0 === 0") {
    assert(pascal(-1,0) === 0)
  }
  
  test("col > row === 0") {
    assert(pascal(5,4) === 0)
  }
  
  /* Pascal's Triangle */
  val rows = List(
    List(1), 
    List(1, 1), 
    List(1, 2, 1), 
    List(1, 3, 3, 1), 
    List(1, 4, 6, 4, 1), 
    List(1, 5, 10, 10, 5, 1), 
    List(1, 6, 15, 20, 15, 6, 1), 
    List(1, 7, 21, 35, 35, 21, 7, 1), 
    List(1, 8, 28, 56, 70, 56, 28, 8, 1), 
    List(1, 9, 36, 84, 126, 126, 84, 36, 9, 1), 
    List(1, 10, 45, 120, 210, 252, 210, 120, 45, 10, 1)
  )

  for (r <- rows.indices)
    for (c <- rows(r).indices)
      test(s"pascal($c, $r) === ${rows(r)(c)}") {
        assert(pascal(c,r) === rows(r)(c))
      }
  
  val balanced = List(
    "(one(two())three)",
    "no parens",
    "()",
    "(foo)"
  )
  
  for (s <- balanced)  
    test(s) {
      assert(balance(s.toList))
    }
  
  val unbalanced = List(
    "(onetwo())three)", // more closes than opens
    "(hi(there)", // more opens than close
    ")",
    "(",
    "correct number( but out) of )( order"
  )
  
  for (s <- unbalanced)  
    test(s) {
      assert(!balance(s.toList))
    }
  
  test("Count change -1 by any list") {
    assert(countChange(-1, List(1,2)) === 0)
  }
  
  test("Count change 15 by 13, 16") {
    assert(countChange(15, List(13, 16)) === 0)
  }
  
  test("Count change 0 by any list") {
    assert(countChange(0, List(1,2)) === 1)
  }
  
  test("Count change 4 by 1,2") {
    assert(countChange(4, List(1,2)) === 3)
  }
  
  test("Count change 15 by 1,3,5") {
    assert(countChange(15, List(1,6,7)) === 6)
  }
  
  test("Count change 15 by 1,3,5,20") {
    assert(countChange(15, List(1,6,7)) === 6)
  }
  
  test("Count change dollar by 1,5,10,25") {
    assert(countChange(100, List(1,5,10,25)) === 242)
  }
}
