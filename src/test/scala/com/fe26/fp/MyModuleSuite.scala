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

package com.fe26.fp

import org.scalatest.FunSpec

/**
  * .
  *
  * @author bilalwahla
  */
class MyModuleSuite extends FunSpec {
  describe("Factorial") {
    describe("When calculating factorial of 5") {
      it("Should be 120") {
        assert(MyModule.factorial(5) == 120)
      }
    }

    describe("When formatting a factorial of 5") {
      it("Should be 'The factorial of 5 is 120'") {
        assert(MyModule.formatFactorial(5) == "The factorial of 5 is 120")
      }
    }
  }

  describe("Fibonacci") {
    describe("When calculating 6th fibonacci") {
      it("Should be 5") {
        assert(MyModule.fib(6) == 5)
      }
    }

    describe("When formatting a 6th fibonacci") {
      it("Should be 'The 6th fibonacci is 5'") {
        assert(MyModule.formatFib(6) == "The 6th fibonacci is 5")
      }
    }
  }

  describe("Abs") {
    describe("When calculating absolute value for -1") {
      it("Should be 1") {
        assert(MyModule.abs(-1) == 1)
      }
    }

    describe("When calculating absolute value for 1") {
      it("Should be 1") {
        assert(MyModule.abs(1) == 1)
      }
    }

    describe("When formatting a absolute value of -100") {
      it("Should be 'The absolute value of -100 is 100'") {
        assert(MyModule.formatAbs(-100) == "The absolute value of -100 is 100")
      }
    }
  }

  describe("Format result") {
    describe("When formatting a factorial of 5") {
      it("Should be 'The result of 5 is 120'") {
        assert(MyModule.formatResult(5, MyModule.factorial) == "The result of 5 is 120")
      }
    }

    describe("When formatting a 6th fibonacci") {
      it("Should be 'The result of 6 is 5'") {
        assert(MyModule.formatResult(6, MyModule.fib) == "The result of 6 is 5")
      }
    }

    describe("When formatting a absolute value of -100") {
      it("Should be 'The result of -100 is 100'") {
        assert(MyModule.formatResult(-100, MyModule.abs) == "The result of -100 is 100")
      }
    }
  }
}
