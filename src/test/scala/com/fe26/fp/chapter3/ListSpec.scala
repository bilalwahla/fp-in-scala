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
      it("Should return 0.0 if any element of the list is 0.0") {
        assert(product(List(1, 2, 3, 0.0)) == 0.0)
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
      it("Should return size of 0") {
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

  describe("Fold right") {
    describe("When we apply a sum function to a list of integers with 0 for default") {
      val l = List(1, 2, 3, 4)
      /*
      2nd group of parameter can be written as:
       (x, y) => x + y
       OR even
       (x: Int, y: Int) => x + y
       The we've done it here, type is inferred from the first group of parameters. And when each
       parameter is used once in the expression we can replace it with underscores.
       */
      val res = foldRight(l, 0)(_ + _)
      it("Should return sum of all elements of the list") {
        assert(res == 10)
      }
      it("Should also return the same result when calling sum2 which is same as above test") {
        assert(sum2(l) == 10)
      }
    }

    describe("When we apply a product function to a list of integers with 0 for default") {
      val l = List(1.0, 2, 3, 4)
      val res = foldRight(l, 1.0)(_ * _)
      it("Should return product of all elements of the list") {
        assert(res == 24)
      }
      it("Should return 0 if list of elements contain a 0") {
        assert(foldRight(List(1.0, 2, 3, 4, 0), 0.0)(_ * _) == 0)
      }
      it("Should return product of all elements of the list even when calling product2") {
        assert(product2(l) == 24)
      }
      it("Should return 0 if list of elements contain a 0 even when calling product2") {
        assert(product2(List(1, 2, 3, 4, 0.0)) == 0)
      }
    }
  }

  describe("Length") {
    describe("When requesting for length of a list") {
      val l = List(1, 2, 3, 4)
      it("Should return correct length (using foldRight) of the given list") {
        assert(length(l) == 4)
      }
      it("Should not mutate the original list computing length (using foldRight)") {
        assert(l == List(1, 2, 3, 4))
      }
      it("Should return correct length (using foldLeft) of the given list") {
        assert(length2(l) == 4)
      }
      it("Should not mutate the original list computing length (using foldLeft)") {
        assert(l == List(1, 2, 3, 4))
      }
    }

    describe("When asking for length of an empty list") {
      val emptyList = List()
      it("Should return length (using foldRight) of 0") {
        assert(length(emptyList) == 0)
      }
      it("Should return length (using foldLeft) of 0") {
        assert(length2(emptyList) == 0)
      }
    }
  }

  describe("Fold left") {
    describe("When we apply a sum function to a list of integers with 0 for default") {
      val l = List(1, 2, 3, 4)
      val res = foldLeft(l, 0)(_ + _)
      it("Should return sum of all elements of the list") {
        assert(res == 10)
      }
      it("Should also return the same result when calling sum3 which is same as above test") {
        assert(sum3(l) == 10)
      }
    }

    describe("When we apply a product function to a list of integers with 0 for default") {
      val l = List(1.0, 2, 3, 4)
      val res = foldLeft(l, 1.0)(_ * _)
      it("Should return product of all elements of the list") {
        assert(res == 24)
      }
      it("Should return 0 if list of elements contain a 0") {
        assert(foldLeft(List(1.0, 2, 3, 4, 0), 0.0)(_ * _) == 0)
      }
      it("Should return product of all elements of the list even when calling product3") {
        assert(product3(l) == 24)
      }
      it("Should return 0 if list of elements contain a 0 even when calling product3") {
        assert(product3(List(1, 2, 3, 4, 0.0)) == 0)
      }
    }
  }

  describe("Reverse - using a fold") {
    describe("When a list of integers is requested to be reversed") {
      val l = List(1, 2, 3)
      val res = reverse(l)
      it("Should return correctly reversed list") {
        assert(res == List(3, 2, 1))
      }
    }

    describe("When an empty list is requested to be reversed") {
      val l = List()
      val res = reverse(l)
      it("Should return the empty list") {
        assert(res == List())
      }
    }
  }

  describe("Append - using fold") {
    describe("When a list is appended to another") {
      val a1 = List(1, 2, 3, 4)
      val a2 = List(5, 6, 7, 8)
      val res = append2(a1, a2)
      it("Should return a list that has been appended with the other") {
        assert(res == List(1, 2, 3, 4, 5, 6, 7, 8))
      }
      it("Should not mutate the original lists") {
        assert(a1 == List(1, 2, 3, 4))
        assert(a2 == List(5, 6, 7, 8))
      }
    }
  }

  describe("Concatenate") {
    describe("When a list of lists is concatenated in to a single list") {
      val l = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
      val res = concatenate(l)
      it("Should return a single list with elements in all lists in the list") {
        assert(res == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
      }
      it("Should not mutate the original list") {
        assert(l == List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
      }
    }

    describe("When a list of empty lists is requested to be concatenated") {
      val l = List(List(), List())
      val res = concatenate(l)
      it("Should return an empty list") {
        assert(res == Nil)
      }
    }
  }

  describe("Transform") {
    describe("When an integer list is requested to be transformed by adding 1 to each of its elements") {
      val l = List(1, 2, 3)
      val res = addOne(l)
      it("Should return a transformed integer list with 1 added to each of its elements") {
        assert(res == List(2, 3, 4))
      }
      it("Should not mutate existing integer list") {
        assert(l == List(1, 2, 3))
      }
    }

    describe("When a double list is requested to be transformed by turning each value into a String") {
      val l = List(1.0, 2.0, 3.0)
      val res = convertToString(l)
      it("Should return a transformed list of strings representing double values in the original list") {
        assert(res == List("1.0", "2.0", "3.0"))
      }
      it("Should not mutate existing list of doubles") {
        assert(l == List(1.0, 2.0, 3.0))
      }
    }
  }

  describe("Map - transformations generalised") {
    describe("When an integer list is requested to be transformed by adding 1 to each of its elements") {
      val l = List(1, 2, 3)
      val res = map(l)(_ + 1)
      it("Should return a transformed integer list with 1 added to each of its elements") {
        assert(res == List(2, 3, 4))
      }
      it("Should not mutate existing integer list") {
        assert(l == List(1, 2, 3))
      }
    }

    describe("When a double list is requested to be transformed by turning each value into a String") {
      val l = List(1.0, 2.0, 3.0)
      val res = map(l)(_.toString)
      it("Should return a transformed list of strings representing double values in the original list") {
        assert(res == List("1.0", "2.0", "3.0"))
      }
      it("Should not mutate existing list of doubles") {
        assert(l == List(1.0, 2.0, 3.0))
      }
    }
  }

  describe("Filter") {
    val l = List(1, 2, 3, 4, 5, 6, 7)
    describe("When it is requested to remove all even numbers from an integer list") {
      val res = filter(l)(_ % 2 != 0)
      it("Should return an integer list excluding all even numbers") {
        assert(res == List(1, 3, 5, 7))
      }
      it("Should not mutate original list") {
        assert(l == List(1, 2, 3, 4, 5, 6, 7))
      }
    }

    describe("When it is requested to remove all even numbers from an integer list (using flatMap)") {
      val res = filter2(l)(_ % 2 != 0)
      it("Should return an integer list excluding all even numbers (using flatMap)") {
        assert(res == List(1, 3, 5, 7))
      }
      it("Should not mutate original list (using flatMap)") {
        assert(l == List(1, 2, 3, 4, 5, 6, 7))
      }
    }
  }

  describe("Flat map") {
    describe("When a list is requested to be doubled up i.e. each element in the list should appear twice") {
      val l = List(1, 2, 3)
      val res = flatMap(l)(i => List(i, i))
      it("Should return the doubled up list") {
        assert(res == List(1, 1, 2, 2, 3, 3))
      }
      it("Should not mutate the original list") {
        assert(l == List(1, 2, 3))
      }
    }
  }
}
