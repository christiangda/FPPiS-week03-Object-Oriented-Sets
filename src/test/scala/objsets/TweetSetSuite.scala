package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)

    /**
     * Testing higth volume of tweets
     */
    val googleList = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
    val appleList = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

    val googleTweets: TweetSet = TweetReader.allTweets.filter { t => googleList.exists { w => t.text.contains(w) } }
    val appleTweets: TweetSet = TweetReader.allTweets.filter { t => appleList.exists { w => t.text.contains(w) } }
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("isEmpty: on empty set") {
    new TestSets {
      assert(set1.isEmpty === true)
    }
  }

  test("mostRetweeted: on empty set") {
    new TestSets {
      intercept[NoSuchElementException] {
        val ret = set1.mostRetweeted
      }
    }
  }

  test("descendingByRetweet: on empty set") {
    new TestSets {
      assert(set1.descendingByRetweet === Nil)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("isEmpty: on set5") {
    new TestSets {
      assert(set5.isEmpty === false)
    }
  }

  test("mostRetweeted: on set5") {
    new TestSets {
      val t = set5.mostRetweeted
      assert(t.text == "a body")
    }
  }

  test("descendingByRetweet: on set5") {
    new TestSets {
      val t = set5.descendingByRetweet
      val o = new Tweet("a", "a body", 20)
      assert(!t.isEmpty)
      assert(t.head.user === o.user)
      assert(t.head.text === o.text)
    }
  }

  test("filter: retweets == 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("filter: retweets == 9 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 9)) === 1)
    }
  }

  test("filter: text == 'a body' on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.text == "a body")) === 1)
    }
  }

  test("filter: user == 'c' on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "c")) === 1)
    }
  }

  test("filter: size(googleTweets) === 38 on googleTweets") {
    new TestSets {
      assert(size(googleTweets) === 38)
    }
  }

  test("filter: size(appleTweets) === 150 on appleTweets") {
    new TestSets {
      assert(size(appleTweets) === 150)
    }
  }

  /**
   * Union
   */
  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("union: googleTweets and appleTweets") {
    new TestSets {
      val allTweetsGnA = googleTweets.union(appleTweets)
      assert(allTweetsGnA.isEmpty === false)
      assert(size(allTweetsGnA) === 179)
    }
  }

  test("descending: on empty set") {
    new TestSets {
      val trends = set1.descendingByRetweet
      intercept[NoSuchElementException] {
        trends.head.user
      }
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("descendingByRetweet: googleTweets.union(appleTweets).descendingByRetweet") {
    new TestSets {
      val allTweetsDes = googleTweets.union(appleTweets).descendingByRetweet
      assert(allTweetsDes.isEmpty === false)
      assert(allTweetsDes.head.retweets >= 1)
    }
  }
}
