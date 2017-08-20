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

package com.fe26.fp.chapter3

import org.scalatest.FunSpec
import com.fe26.fp.chapter3.List._

/**
  * Test specification for our `List`. Representing BDD using FunSpec style.
  *
  * @author bilalwahla
  */
class ListSpec extends FunSpec {
  describe("Pattern matching") {
    describe("When pattern matching on a list of 5 integers") {
      val res: Int = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
      }
      it("Should return first matching case as the result") {
        assert(res == 3)
      }
    }
  }

  describe("Sum") {
    describe("When a list of integers is summed up") {
      it("Should return correct sum of all elements of the list") {
        assert(sum(List(1, 2, 3, 4, 5, 6)) == 21)
      }
    }
  }

  describe("Product") {
    describe("When a list of doubles is multiplied") {
      it("Should return correct product of all elements in the list") {
        assert(product(List(1.0, 2, 3, 4)) == 24)
      }
    }
  }

  describe("Append") {
    describe("When a list is appended to another") {
      val a1 = List(1, 2, 3, 4)
      val a2 = List(5, 6, 7, 8)
      val res = append(a1, a2)
      it("Should return a list that has been appended with the other") {
        assert(res == List(1, 2, 3, 4, 5, 6, 7, 8))
      }
      it("Should not mutate the original lists") {
        assert(a1 == List(1, 2, 3, 4))
        assert(a2 == List(5, 6, 7, 8))
      }
    }
  }

  describe("Size") {
    describe("When requesting for size of a list") {
      val l = List(1, 2, 3, 4)
      val res = size(l)
      it("Should return correct size of the given list") {
        assert(res == 4)
      }
      it("Should not mutate the original list") {
        assert(l == List(1, 2, 3, 4))
      }
    }

    describe("When asking for size of an empty list") {
      val emptyList = List()
      val res = size(emptyList)
      it("Should return size§ of 0") {
        assert(res == 0)
      }
    }
  }

  describe("Tail") {
    describe("When list is requested without its first element in it") {
      val l = List(1, 2, 3, 4)
      val res = tail(l)
      it("Should return list excluding the first element") {
        assert(res == List(2, 3, 4))
      }
      it("Should not mutate the original list") {
        assert(l == List(1, 2, 3, 4))
      }
    }

    describe("When an empty list is requested without its first element in it") {
      val emptyList = List()
      val res = tail(emptyList)
      it("Should return an empty list") {
        assert(res == Nil)
      }
    }
  }

  describe("Head") {
    describe("When list is requested for its first element only") {
      val l = List(1, 2, 3, 4)
      val res = head(l)
      it("Should return the first element") {
        assert(res == 1)
      }
      it("Should not mutate the original list") {
        assert(l == List(1, 2, 3, 4))
      }
    }

    describe("When an empty list is requested for its first element only") {
      val emptyList = List()
      it("Should return an empty list") {
        intercept[RuntimeException] {
          head(emptyList)
        }
      }
    }
  }

  describe("Set head") {
    describe("When replace the first element of the list with a different value") {
      val l = List(1, 2, 3, 4)
      val res = setHead(l, 0)
      it("Should return list with updated value of the first element") {
        assert(res == List(0, 2, 3, 4))
      }
      it("Should not mutate the original list") {
        assert(l == List(1, 2, 3, 4))
      }
    }

    describe("When replace the first element of an empty list") {
      val emptyList = List()
      it("Should cause a runtime exception") {
        intercept[RuntimeException] {
          setHead(emptyList, 0)
        }
      }
    }
  }

  describe("Drop") {
    val l = List(1, 2, 3, 4)
    describe(" When asked to remove first n elements from the given list") {
      val res = drop(l, 2)
      it("Should return list with first n elements removed") {
        assert(res == List(3, 4))
      }
      it("Should not mutate the original list") {
        assert(l == List(1, 2, 3, 4))
      }
    }
  }

  describe("Drop while") {
    val p = (n: Int) => n % 2 == 0
    describe("When requested to remove elements from a list prefix based on a predicate") {
      val l = List(0, 2, 3, 4, 5, 6, 7, 8, 9)
      val res = dropWhile(l, p)
      it("Should remove all list prefix elements that match predicate") {
        assert(res == List(3, 4, 5, 6, 7, 8, 9))
      }
      it("Should not mutate the original list") {
        assert(l == List(0, 2, 3, 4, 5, 6, 7, 8, 9))
      }
    }

    describe("When requested to remove elements from a list prefix where whole list matches predicate") {
      val l = List(0, 2, 4, 6, 8)
      val res = dropWhile(l, p)
      it("Should drop all elements returning an empty list") {
        assert(res == Nil)
      }
    }
  }

  describe("Get Nth element") {
    describe("When nth element of a list is requested") {
      val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
      val res = get(7, l)
      it("Should return the nth element") {
        assert(res == 8)
      }
    }

    describe("When nth element of an empty list is requested") {
      val emptyList = List()
      it("Should cause an out of bounds error") {
        intercept[RuntimeException] {
          get(0, emptyList)
        }
      }
    }
  }

  describe("Init") {
    describe("When passing in a list") {
      val l = List(1, 2, 3, 4)
      val res = init(l)
      it("Should return all but the last element of the given list") {
        assert(res == List(1, 2, 3))
      }
    }

    describe("When init on an empty list") {
      val emptyList = List()
      it("Should cause an error") {
        intercept[RuntimeException] {
          init(emptyList)
        }
      }
    }
  }
}
