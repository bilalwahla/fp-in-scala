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

package com.fe26.fp.chapter2

import org.scalatest.FunSpec
import com.fe26.fp.chapter2.MyModule._

/**
  * Test specification for MyModule. Representing BDD using FunSpec style.
  *
  * @author bilalwahla
  */
class MyModuleSpec extends FunSpec {
  describe("Factorial") {
    describe("When calculating factorial of 5") {
      it("Should be 120") {
        assert(factorial(5) == 120)
      }
    }

    describe("When formatting a factorial of 5") {
      it("Should be 'The factorial of 5 is 120'") {
        assert(formatFactorial(5) == "The factorial of 5 is 120")
      }
    }
  }

  describe("Fibonacci") {
    describe("When calculating 6th fibonacci") {
      it("Should be 8") {
        assert(fib(6) == 8)
      }
    }

    describe("When formatting a 6th fibonacci") {
      it("Should be 'The 6th fibonacci is 8'") {
        assert(formatFib(6) == "The 6th fibonacci is 8")
      }
    }
  }

  describe("Abs") {
    describe("When calculating absolute value for -1") {
      it("Should be 1") {
        assert(abs(-1) == 1)
      }
    }

    describe("When calculating absolute value for 1") {
      it("Should be 1") {
        assert(abs(1) == 1)
      }
    }

    describe("When formatting a absolute value of -100") {
      it("Should be 'The absolute value of -100 is 100'") {
        assert(formatAbs(-100) == "The absolute value of -100 is 100")
      }
    }
  }

  describe("Format result") {
    describe("When formatting a factorial of 5") {
      it("Should be 'The result of 5 is 120'") {
        assert(formatResult(5, factorial) == "The result of 5 is 120")
      }
    }

    describe("When formatting a 6th fibonacci") {
      it("Should be 'The result of 6 is 8'") {
        assert(formatResult(6, fib) == "The result of 6 is 8")
      }
    }

    describe("When formatting a absolute value of -100") {
      it("Should be 'The result of -100 is 100'") {
        assert(formatResult(-100, abs) == "The result of -100 is 100")
      }
    }
  }

  describe("Monomorphic: find first") {
    describe("When finding first occurrence of key in an array of strings with an occurrence") {
      it("Should be able to return index of the occurrence") {
        assert(findFirst(Array("Audi", "A5", "Sportback", "1.8", "TFSI"), "A5") == 1)
      }
    }

    describe("When finding first occurrence of key in an array of strings with 2 occurrences") {
      it("Should be able to return index of the first occurrence") {
        assert(findFirst(Array("Audi", "A5", "Sportback", "1.8", "TFSI", "A5"), "A5") == 1)
      }
    }

    describe("When finding first occurrence of key in an array of strings with 0 occurrence") {
      it("Should return -1") {
        assert(findFirst(Array("Audi", "A5", "Sportback", "1.8", "TFSI"), "R8") == -1)
      }
    }
  }

  describe("Polymorphic: find first occurrence") {
    describe("When finding first occurrence for a condition in any array with an occurrence") {
      it("Should be able to return index of the occurrence") {
        assert(findFirst(Array(1, 2, 3, 4, 5, 6), (x: Int) => x == 4) == 3)
      }
    }

    describe("When finding first occurrence for a condition in an array of strings with 2 occurrences") {
      it("Should be able to return index of the first occurrence") {
        assert(findFirst(Array(1, 2, 3, 4, 5, 6, 4), (x: Int) => x == 4) == 3)
      }
    }

    describe("When finding first occurrence of key in an array of strings with 0 occurrence") {
      it("Should return -1") {
        assert(findFirst(Array(1, 2, 3, 4, 5, 6), (x: Int) => x == 9) == -1)
      }
    }
  }

  describe("Polymorphic: is sorted") {
    describe("When checking if a sorted array is sorted") {
      it("Should return true") {
        assert(isSorted(Array(1, 2, 3, 4, 5, 6), (x: Int, y: Int) => x < y))
      }
    }

    describe("When checking if a sorted string array is sorted") {
      it("Should return true") {
        assert(isSorted(Array("abc", "bcd", "cde", "def", "efg"), (s1: String, s2: String) => s1 < s2))
      }
    }

    describe("When checking if a unsorted array is sorted") {
      it("Should return false") {
        assert(!isSorted(Array(1, 2, 3, 4, 5, 6, 0), (x: Int, y: Int) => x < y))
      }
    }

    describe("When checking if a unsorted string array is sorted") {
      it("Should return false") {
        assert(!isSorted(Array("abc", "bcd", "cde", "aaa", "def", "efg"), (s1: String, s2: String) => s1 < s2))
      }
    }
  }

  describe("Partial") {
    describe("When an integer and a function that is a multiple of the integer and the other parameter, are passed in") {
      it("should return a function that when passed another integer returns the multiple of the two") {
        assert(partial(5, (a: Int, b: Int) => a * b)(5) == 25)
      }
    }
  }

  describe("Currying") {
    describe("When a function that returns a multiple of the two parameters, is passed in") {
      it("Should return a function that takes the first parameter value and return a function that takes the second parameter value") {
        assert(curry((a: Int, b: Int) => a * b)(5)(5) == 25)
      }
    }
  }

  describe("Uncurrying") {
    describe("When a curried function is passed in that takes an integer and returns a function that takes an integer and returns a multiple of the two") {
      it("Should return a function that simply takes two integers and returns a product") {
        assert(uncurry((a: Int) => (b: Int) => a * b)(5, 5) == 25)
      }
    }
  }

  describe("Function composition") {
    describe("When two functions are passed in each taking an integer and returning a square") {
      it("Should return a function that takes an integer, applies it to one function and feeds the result to another producing the final result") {
        assert(compose((b: Int) => b * b, (a: Int) => a * a)(2) == 16)
      }
    }
  }
}
