package funsets

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.collection.immutable.Range

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    // singletons
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    
    // multivalued sets
    private val r3To6By1 = new Range(3, 7, 1)
    private val r5To8By1 = new Range(5, 9, 1)
    private val r2To10By2 = new Range(2, 11, 2)

    def s3To6By1(x: Int):Boolean = r3To6By1.contains(x)
    def s5To8By1(x: Int):Boolean = r5To8By1.contains(x)
    def s2To10By2(x: Int):Boolean = r2To10By2.contains(x)
    
    val setZeroToFive = (x: Int) => x >= 0 && x <= 5
    val setFiveToTen  = (x: Int) => x >= 5 && x <= 10
    val setFourToSix  = (x: Int) => x >= 4 && x <= 6
    val emptySet      = (x: Int) => x == 0 && x != 0
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("diff overlap") {
    new TestSets {
      val s = diff(s3To6By1, s5To8By1)
      assert(contains(s, 3), "diff 3")
      assert(contains(s, 4), "diff 4")
      assert(!contains(s, 5), "diff 5")
      assert(!contains(s, 6), "diff 6")
      assert(!contains(s, 7), "diff 7")
      assert(!contains(s, 8), "diff 8")
    }
  }

  test("diff evens") {
    new TestSets {
      val s = diff(s2To10By2, s3To6By1)
      assert(contains(s, 2), "diff 2") // in s2To10By2 not in s3To6By1
      assert(!contains(s, 3), "diff 3") // not in s2To10By2 in s3To6By1
      assert(!contains(s, 4), "diff 4") // in s2To10By2 and in s3To6By1
    }
  }

  test("filter evens") {
    new TestSets {
      assert(!contains(filter(s2To10By2, x=>x%2==0), 0), "filter 0")
      assert(!contains(filter(s2To10By2, x=>x%2==0), 1), "filter 1")
      assert(contains(filter(s2To10By2, x=>x%2==0), 2), "filter 2")
      assert(!contains(filter(s2To10By2, x=>x%2==0), 3), "filter 3")
      assert(contains(filter(s2To10By2, x=>x%2==0), 4), "filter 4")
      assert(!contains(filter(s2To10By2, x=>x%2==0), 5), "filter 5")
      assert(contains(filter(s2To10By2, x=>x%2==0), 6), "filter 6")
      assert(!contains(filter(s2To10By2, x=>x%2==0), 7), "filter 7")
      assert(contains(filter(s2To10By2, x=>x%2==0), 8), "filter 2")
    }
  }

  test("forall") {
    new TestSets {
      assert(forall(s2To10By2, x=>x%2==0), "forall evens") // all members of set are even
      assert(forall(s2To10By2, x=>x<11), "forall less than 11") // all members of set are <11
      assert(!forall(s2To10By2, x=>x<8), "forall less than 8") // 8 is in set but not <8
    }
  }

  test("exists") {
    new TestSets {
      assert(exists(s2To10By2, x=>x%2==0), "exists even") // any one of the members of the set are even
      assert(!exists(s2To10By2, x=>x%2==1), "exists odd") // no member of set is odd
    }
  }

  test("map") {
    new TestSets {
      assert(contains(map(s2To10By2, {x=>x+1}), 11), "map evens to odds") // 10 from set will transform to 11
      assert(!contains(map(s2To10By2, {x=>x+2}), 3), "map evens to more evens") // no member of set will transform to 3
    }
  }
    test("exists returns true if any element in the set satisfies the given predicate"){
    new TestSets {
      val e1 = exists(setFourToSix, (x: Int) => x == 4)
      assert(e1 === true)
    }
  }

  test("exists returns false if no elements in the set satisfy the given predicate"){
    new TestSets {
      val e2 = exists(setFourToSix, (x: Int) => x > 6)
      assert(e2 === false)
    }
  }
}
