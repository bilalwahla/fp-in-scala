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

package com.fe26.fp.chapter5

import org.scalatest.FreeSpec
import com.fe26.fp.chapter5.Stream._

/**
  * Test specification for `Stream`.
  *
  * @author bilalwahla
  */
class StreamSpec extends FreeSpec {
  val emptyStream: Stream[Int] = empty
  val s1 = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9)
  val s2 = Stream(10, 11)
  val l1 = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  val l2 = List(2, 3, 4, 5, 6, 7, 8, 9, 10)
  val l3 = List(1, 3, 5, 7, 9)
  val l4 = List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9)
  val l5 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
  val l6 = List(10, 11)

  "Head option" - {
    "When retrieving head from an empty stream" - {
      "Should return an option of none" in {
        assert(emptyStream.headOption.isEmpty)
      }
    }
    "When retrieving head from a non empty stream" - {
      "Should return the head of the stream" in {
        assert(s1.headOption.contains(1))
      }
    }
    "When retrieving head (headOption2) from an empty stream" - {
      "Should return an option of none" in {
        assert(emptyStream.headOption2.isEmpty)
      }
    }
    "When retrieving head (headOption2) from a non empty stream" - {
      "Should return the head of the stream" in {
        assert(s1.headOption2.contains(1))
      }
    }
  }

  "To List" - {
    "When converting an empty stream to a list" - {
      "Should return an empty list" in {
        assert(emptyStream.toList == Nil)
      }
    }
    "When converting a non empty stream to a list" - {
      "Should return a list with all elements in the stream" in {
        assert(s1.toList == l1)
      }
    }
  }

  "Take" - {
    "While retrieving first 5 elements of an empty stream" - {
      "Should return an empty stream" in {
        assert(emptyStream.take(5) == empty)
      }
    }
    "While retrieving first 3 elements of a stream" - {
      "Should correctly return the first 3 elements of the given stream" in {
        assert(s1.take(3).toList == List(1, 2, 3))
      }
    }
  }

  "Drop" - {
    "While attempting to drop first 2 elements of an empty stream" - {
      "Should return an empty stream" in {
        assert(emptyStream.drop(2) == empty)
      }
    }
    "While retrieving stream from a stream with first 3 elements dropped" - {
      "Should get a stream with first 3 elements of the original stream dropped" in {
        assert(s1.drop(3).toList == List(4, 5, 6, 7, 8, 9))
      }
      "Should not mutate the original stream" in {
        assert(s1.toList == l1)
      }
    }
  }

  "Take while" - {
    "While taking only odd elements from a stream of integers" - {
      "Should return the odd elements" in {
        assert(s1.takeWhile(_ % 2 != 0).toList == List(1))
      }
      "Should not mutate the original stream" in {
        assert(s1.toList == l1)
      }
    }
    "While taking only odd elements from an empty stream" - {
      "Should return an empty stream" in {
        assert(emptyStream.takeWhile(_ % 2 != 0) == empty)
      }
    }
    "While taking (takeWhile2) only odd elements from a stream of integers" - {
      "Should return the odd elements" in {
        assert(s1.takeWhile2(_ % 2 != 0).toList == List(1))
      }
      "Should not mutate the original stream" in {
        assert(s1.toList == l1)
      }
    }
    "While taking (takeWhile2) only odd elements from an empty stream" - {
      "Should return an empty stream" in {
        assert(emptyStream.takeWhile2(_ % 2 != 0) == empty)
      }
    }
  }

  "Exists" - {
    "While checking existence of an element in a stream of integers with that element in there" - {
      "Should return true" in {
        assert(s1.exists(_ >= 9))
      }
    }
    "While checking existence of an element in an empty stream" - {
      "Should return false" in {
        assert(!emptyStream.exists(_ == 1))
      }
    }
    "While checking existence (exists2) of an element in a stream of integers with that element in there" - {
      "Should return true" in {
        assert(s1.exists2(_ >= 9))
      }
    }
    "While checking existence (exists2) of an element in an empty stream" - {
      "Should return false" in {
        assert(!emptyStream.exists2(_ == 1))
      }
    }
  }

  "Fold right" - {
    "While computing a sum of a stream of integers using fold right with a default value of 0" - {
      "Should return correct sum" in {
        assert(s1.foldRight(0)(_ + _) == 45)
      }
      "Should not mutate the existing stream" in {
        assert(s1.toList == l1)
      }
    }
    "While computing a sum of an empty stream using fold right with a default value of 0" - {
      "Should return the default value of 0" in {
        assert(emptyStream.foldRight(0)(_ + _) == 0)
      }
    }
  }

  "For all" - {
    "While checking whether all elements in a stream of integers (all matching predicate) match the given predicate" - {
      "Should return true" in {
        assert(s1.forAll(_ > 0))
      }
    }
    "While checking whether all elements in an empty stream of integers match the given predicate" - {
      "Should return true" in {
        assert(emptyStream.forAll(_ > 0))
      }
    }
  }

  "Map" - {
    "When an integer stream is requested to be transformed by adding 1 to each of its elements" - {
      "Should return a transformed integer stream with 1 added to each of its elements" in {
        assert(s1.map(_ + 1).toList == l2)
      }
    }
    "When an empty stream is requested to be transformed by adding 1 to each of its elements" - {
      "Should return an empty stream" in {
        assert(emptyStream.map(_ + 1) == empty)
      }
    }
  }

  "Filter" - {
    "When it is requested to remove all even numbers from an integer stream" - {
      "Should return an integer stream excluding all even numbers" in {
        assert(s1.filter(_ % 2 != 0).toList == l3)
      }
    }
    "When it is requested to remove all even numbers from an empty stream" - {
      "Should return an empty" in {
        assert(emptyStream.filter(_ % 2 != 0) == empty)
      }
    }
  }

  "Flat map" - {
    "When a stream is requested to be doubled up i.e. each element in the stream should appear twice" - {
      "Should return the doubled up stream" in {
        assert(s1.flatMap(a => cons(a, cons(a, empty))).toList == l4)
      }
    }
    "When an empty stream is requested to be doubled up" - {
      "Should return an empty stream" in {
        assert(emptyStream.flatMap(a => cons(a, empty)) == empty)
      }
    }
  }

  "Append" - {
    "When a stream is appended to another" - {
      "Should return a stream that has been appended with the other" in {
        assert(s1.append(s2).toList == l5)
      }
    }
    "When an empty stream is appended to another" - {
      "Should return the stream being appended" in {
        assert(emptyStream.append(s2).toList == l6)
      }
    }
  }

  "Find" - {
    "When we try to find an element in an integer stream" - {
      "Should return the found element" in {
        assert(s1.find(_ % 5 == 0).contains(5))
      }
    }
    "When we try to find an element in an empty stream" - {
      "Should return an none" in {
        assert(emptyStream.find(_ % 5 == 0).isEmpty)
      }
    }
  }

  "Infinite streams" - {
    "When inspected" - {
      "Should only inspect the portion of the stream needed to generate the demanded output" in {
        assert(ones.take(5).toList == List(1, 1, 1, 1, 1))
        assert(ones.map(_ + 1).exists(_ % 2 == 0))
        assert(!ones.forAll(_ != 1))
        assert(ones2.take(5).toList == List(1, 1, 1, 1, 1))
        assert(ones2.map(_ + 1).exists(_ % 2 == 0))
        assert(!ones2.forAll(_ != 1))
        /*
        Below will never encounter an element that allows it to terminate with a definite answer
        (this will manifest as a stack overflow rather than an infinite loop).
         */
        //ones.takeWhile(_ == 1).toList

        // OR

        //ones.forall(_ == 1)
      }
    }
    "When a constant is created of an infinite stream of 3s" - {
      "Should return an infinite stream of 3s" in {
        assert(constant(3).take(5).toList == List(3, 3, 3, 3, 3))
        assert(constant2(3).take(5).toList == List(3, 3, 3, 3, 3))
        assert(constant3(3).take(5).toList == List(3, 3, 3, 3, 3))
      }
    }
    "When a constant is created of an infinite stream of Xs" - {
      "Should return an infinite stream of Xs" in {
        assert(constant("X").take(5).toList == List("X", "X", "X", "X", "X"))
        assert(constant2("X").take(5).toList == List("X", "X", "X", "X", "X"))
        assert(constant3("X").take(5).toList == List("X", "X", "X", "X", "X"))
      }
    }
    "When creating an infinite stream of integers starting from n and then n + 1 and so on" - {
      "Should return an infinite stream starting from n and then n + 1 and so on" in {
        assert(from(6).take(5).toList == List(6, 7, 8, 9, 10))
        assert(from2(6).take(5).toList == List(6, 7, 8, 9, 10))
      }
    }
    "When creating an infinite stream of fibonacci numbers" - {
      "Should return an infinite stream of fibonacci numbers" in {
        assert(fibs.take(5).toList == List(0, 1, 1, 2, 3))
        assert(fibs2.take(5).toList == List(0, 1, 1, 2, 3))
      }
    }
  }

  "Unfold" - {
    "When creating an infinite stream of integers starting from n and then n + 1 and so on" - {
      "Should return an infinite stream starting from n and then n + 1 and so on" in {
        assert(unfold(5)(n => Some((n, n + 1))).take(3).toList == List(5, 6, 7))
        assert(unfold2(5)(n => Some((n, n + 1))).take(3).toList == List(5, 6, 7))
        assert(unfold3(5)(n => Some((n, n + 1))).take(3).toList == List(5, 6, 7))
      }
    }
  }
}
