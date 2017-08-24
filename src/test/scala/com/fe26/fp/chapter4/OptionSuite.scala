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

import org.scalatest.FunSuite

/**
  * Test specification for `Option`. Representing TDD using `FunSuite` style.
  *
  * @author bilalwahla
  */
class OptionSuite extends FunSuite {
  trait Options {
    val s = Some(2.0)
  }

  test("Should be able to double the value of a double option by using a function that doubles it") {
    new Options {
      assert(s.map(v => v * 2) == Some(4))
    }
  }

  test("Should be able to get a value from some option") {
    new Options {
      assert(s.getOrElse(0.0) == 2)
    }
  }

  test("Should get default value from none option") {
    assert(None.getOrElse(0) == 0)
  }

  test("Should be able to retrieve an option by passing in a function that returns an option") {
    new Options {
      assert(s.flatMap(v => Some(v * 2)) == Some(4))
    }
  }

  test("Should be able to get the option from some option") {
    new Options {
      assert(s.orElse(None) == Some(2))
    }
  }

  test("Should be able to get the default value of none from none option") {
    assert(None.orElse(Some("default")) == Some("default"))
  }

  test("Should be able to get the some option if the filter applies") {
    new Options {
      assert(s.filter(_ % 2 == 0) == Some(2))
    }
  }

  test("Should be able to get the none option if the filter does not apply") {
    new Options {
      assert(s.filter(_ % 2 != 0) == None)
    }
  }
}
