package objsets

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {

  trait TestSets {
    val ta = new Tweet("a", "Ape", 20)
    val tb = new Tweet("b", "Bat", 21)
    val tc = new Tweet("c", "Cat", 7)
    val td = new Tweet("d", "Dog", 9)
    val setA = new Empty().incl(ta)
    val setC = new Empty().incl(tc)
    val setCB = setC.incl(tb)
    val setCBA = setCB.incl(ta)
    val setCBD = setCB.incl(td)
    val setCBAD = setCBA.incl(td)
    val setADC = setA.incl(td).incl(tc)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

//  test("Print test data") {
//    new TestSets {
//      println(s"setC    :\t$setC")
//      println(s"setCB   :\t$setCB")
//      println(s"setCBA  :\t$setCBA")
//      println(s"setCBD  :\t$setCBD")
//      println(s"setCBAD :\t$setCBAD")
//    }
//  }
  
  test("filter: on empty set") {
    new TestSets {
      assert(size(new Empty().filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on setCBAD") {
    new TestSets {
      assert(size(setCBAD.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on setCBAD") {
    new TestSets {
      assert(size(setCBAD.filter(tw => tw.retweets >= 20)) === 2)
    }
  }

  test("union: empty with empty") {
    new TestSets {
      assert(size(new Empty().union(new Empty)) === 0)
    }
  }

  test("union: setCBAD with empty") {
    new TestSets {
      val s = setCBAD.union(new Empty)
      assert(size(s) === 4)
      assert(s.contains(ta))
      assert(s.contains(tb))
      assert(s.contains(tc))
      assert(s.contains(td))
    }
  }

  test("union: empty set with setCBAD") {
    new TestSets {
      val s = new Empty().union(setCBAD)
      assert(size(s) === 4)
      assert(s.contains(ta))
      assert(s.contains(tb))
      assert(s.contains(tc))
      assert(s.contains(td))
    }
  }

  test("union: setA and setC") {
    new TestSets {
      val s = setA.union(setC)
      assert(size(s) === 2)
      assert(s.contains(ta))
      assert(s.contains(tc))
    }
  }

  test("union: setA and setCB") {
    new TestSets {
      val s = setA.union(setCB)
      assert(size(s) === 3)
      assert(s.contains(ta))
      assert(s.contains(tb))
      assert(s.contains(tc))
    }
  }

  test("union: setCB and setA") {
    new TestSets {
      val s = setCB.union(setA)
      assert(size(s) === 3)
      assert(s.contains(ta))
      assert(s.contains(tb))
      assert(s.contains(tc))
    }
  }

  test("union: setCBA and setCBD") {
    new TestSets {
      val s = setCBA.union(setCBD)
      assert(size(s) === 4)
      assert(s.contains(ta))
      assert(s.contains(tb))
      assert(s.contains(tc))
      assert(s.contains(td))
    }
  }

  test("most retweets: empty") {
    new TestSets {
      intercept[NoSuchElementException] {
        new Empty().mostRetweeted
      }
    }
  }

  test("most retweets: non-empty") {
    new TestSets {
      assert(tb == setCBAD.mostRetweeted)
    }
  }

  test("descending: empty") {
    new TestSets {
      assert(Nil == new Empty().descendingByRetweet)
    }
  }

  test("descending: non-empty") {
    new TestSets {
      val trends = setCBAD.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head === tb)
      assert(trends.tail.head === ta)
      assert(trends.tail.tail.head === td)
      assert(trends.tail.tail.tail.head === tc)
    }
  }
}
