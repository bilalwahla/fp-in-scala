/*
 * Copyright (c) [2017] [fe26 Limited]
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance
 * with the License.
 *
 * You may obtain a copy of the License at:
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed
 * on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under the License.
 */

package com.fe26.fp.chapter4

import org.scalatest.FreeSpec
import com.fe26.fp.chapter4.Either._

/**
  * Test specification for `Either`. Lets go back to BDD but this time let us use `FreeSpec`.
  *
  * @author bilalwahla
  */
class EitherSpec extends FreeSpec {
  "Mean of an indexed sequence of doubles" - {
    "When calculated for an indexed sequence of doubles" - {
      "Should return an either with correct mean" in {
        assert(mean(IndexedSeq(1.0, 2, 3)) == Right(2))
      }
    }
    "When calculated for an empty sequence" - {
      "Should return an either with an error message" in {
        assert(mean(IndexedSeq()) == Left("mean of empty list!"))
      }
    }
  }

  "Safe division of integers" - {
    "When an integer x is divided by integer y" - {
      val e = safeDiv(9, 3)
      "Should return an either of correct result of the division" in {
        assert(e == Right(3))
      }
    }
    "When an integer x is divided by integer 0" - {
      val e = safeDiv(1, 0)
      "Should return an either with an exception" in {
        assert(e.isInstanceOf[Left[Exception]])
      }
    }
  }

  "Either" - {
    val rightEither1 = Right(10)
    val rightEither2 = Right(20)
    val rightEither3 = Right(30)
    val leftEither1 = Left("Empty")
    val leftEither2 = Left("No such element")

    def f = (x: Int, y: Int) => x + y

    val l1 = List(rightEither1, rightEither2, rightEither3)
    val l2 = List(rightEither1, rightEither2, leftEither1, rightEither3)

    val ints1 = List(10, 20, 30)
    val ints2 = List(20, 40, 60)
    val emptyList = List()

    "When a right either is requested to be doubled using map" - {
      val e = rightEither1 map (_ * 2)
      "Should return doubled right either" in {
        assert(e == rightEither2)
      }
    }
    "When a left either is simply requested using map" - {
      val e = leftEither1 map (e => e)
      "Should return the left either itself" in {
        assert(e == leftEither1)
      }
    }
    "When flat map is passed in with function that returns a right either" - {
      val e = rightEither1 flatMap (a => Right(a * 2))
      "Should be able to retrieve a right either with function applied" in {
        assert(e == rightEither2)
      }
    }
    "When a left either is simply requested using flat map" - {
      val e = leftEither1 flatMap (Left(_))
      "Should return the left either itself" in {
        assert(e == leftEither1)
      }
    }
    "When an either is requested for from a right either, with a default either" - {
      val e = rightEither1 orElse leftEither1
      "Should be able to retrieve the right either" in {
        assert(e == rightEither1)
      }
    }
    "When an either is requested for from a left either, with a default either" - {
      val e = leftEither1 orElse leftEither2
      "Should return the default either" in {
        assert(e == leftEither2)
      }
    }
    "When a function with two arguments is lifted to operate in the context of either after the " +
      "fact with two right either (pattern matching)" - {
      val e = rightEither1.map2(rightEither2)(f)
      "Should return an either with a value after the application of the function" in {
        assert(e == rightEither3)
      }
    }
    "When a function with two arguments is lifted to operate in the context of either after the " +
      "fact with a left and a right either (pattern matching)" - {
      val e = leftEither1.map2(rightEither1)(f)
      "Should return the left either" in {
        assert(e == leftEither1)
      }
    }
    "When a function with two arguments is lifted to operate in the context of either after the " +
      "fact with a right and a left either (pattern matching)" - {
      val e = rightEither1.map2(leftEither1)(f)
      "Should return the left either" in {
        assert(e == leftEither1)
      }
    }
    "When a function with two arguments is lifted to operate in the context of either after the " +
      "fact with two left either (pattern matching)" - {
      val e = leftEither1.map2(leftEither2)(f)
      "Should return the first left either" in {
        assert(e == leftEither1)
      }
    }
    "When a function with two arguments is lifted to operate in the context of either after the " +
      "fact with two right either (map & flatMap)" - {
      val e = rightEither1.map3(rightEither2)(f)
      "Should return an either with a value after the application of the function" in {
        assert(e == rightEither3)
      }
    }
    "When a function with two arguments is lifted to operate in the context of either after the " +
      "fact with a left and a right either (map & flatMap)" - {
      val e = leftEither1.map3(rightEither1)(f)
      "Should return the left either" in {
        assert(e == leftEither1)
      }
    }
    "When a function with two arguments is lifted to operate in the context of either after the " +
      "fact with a right and a left either (map & flatMap)" - {
      val e = rightEither1.map3(leftEither1)(f)
      "Should return the left either" in {
        assert(e == leftEither1)
      }
    }
    "When a function with two arguments is lifted to operate in the context of either after the " +
      "fact with two left either (map & flatMap)" - {
      val e = leftEither1.map3(leftEither2)(f)
      "Should return the first left either" in {
        assert(e == leftEither1)
      }
    }
    "When a function with two arguments is lifted to operate in the context of either after the " +
      "fact with two right either (for-comprehension)" - {
      val e = rightEither1.map4(rightEither2)(f)
      "Should return an either with a value after the application of the function" in {
        assert(e == rightEither3)
      }
    }
    "When a function with two arguments is lifted to operate in the context of either after the " +
      "fact with a left and a right either (for-comprehension)" - {
      val e = leftEither1.map4(rightEither1)(f)
      "Should return the left either" in {
        assert(e == leftEither1)
      }
    }
    "When a function with two arguments is lifted to operate in the context of either after the " +
      "fact with a right and a left either (for-comprehension)" - {
      val e = rightEither1.map4(leftEither1)(f)
      "Should return the left either" in {
        assert(e == leftEither1)
      }
    }
    "When a function with two arguments is lifted to operate in the context of either after the " +
      "fact with two left either (for-comprehension)" - {
      val e = leftEither1.map4(leftEither2)(f)
      "Should return the first left either" in {
        assert(e == leftEither1)
      }
    }
    "When combining a list of right into a right with the values as a list" - {
      val e = sequence(l1)
      "Should return a right with the values as a list" in {
        assert(e == Right(ints1))
      }
    }
    "When combining a list of right and left" - {
      val e = sequence(l2)
      "Should return the first found left" in {
        assert(e == leftEither1)
      }
    }
    "When combining a list of integers into a right with the list as value with given function " +
      "applied to each value" - {
      val e = traverse(ints1)(a => Right(a * 2))
      "Should return right with the list of integers as value with given function applied" in {
        assert(e == Right(ints2))
      }
    }
    "When combining an empty list into a right with the empty list as value" - {
      val e = traverse(emptyList)(Right(_))
      "Should return right with the empty list of as value" in {
        assert(e == Right(emptyList))
      }
    }
  }
}
