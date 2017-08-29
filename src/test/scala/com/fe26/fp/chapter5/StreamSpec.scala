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
  val s1 = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)
  val l1 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)

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
        assert(s1.drop(3).toList == List(4, 5, 6, 7, 8, 9, 0))
      }
      "Should not mutate the original stream" in {
        assert(s1.toList == l1)
      }
    }
  }

  "Take while" - {
    "While taking only odd elements from a stream of integers" - {
      "Should return the odd elements" in {
        assert(s1.takeWhile(_ % 2 != 0).toList == List(1, 3, 5, 7, 9))
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
  }
}
