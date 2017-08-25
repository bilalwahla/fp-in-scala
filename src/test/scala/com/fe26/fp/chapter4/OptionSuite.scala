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
import com.fe26.fp.chapter4.Option._

/**
  * Test specification for `Option`. Representing TDD using `FunSuite` style.
  *
  * @author bilalwahla
  */
class OptionSuite extends FunSuite {
  trait Options {
    def f(a: Double, b: Double): Double = a + b

    val s1 = Some(2.0)
    val s2 = Some(5.0)

    val seq = Seq(1.0, 2)

    val l1 = List(s1, s2)
    val l2 = List(s1, None)
  }

  test("Should be able to double the value of a double option by using a function that doubles it") {
    new Options {
      assert(s1.map(v => v * 2) == Some(4))
    }
  }

  test("Should be able to get a value from some option") {
    new Options {
      assert(s1.getOrElse(0.0) == 2)
    }
  }

  test("Should get default value from none option") {
    assert(None.getOrElse(0) == 0)
  }

  test("Should be able to retrieve an option by passing in a function that returns an option") {
    new Options {
      assert(s1.flatMap(v => Some(v * 2)) == Some(4))
    }
  }

  test("Should be able to get the option from some option") {
    new Options {
      assert(s1.orElse(None) == Some(2))
    }
  }

  test("Should be able to get the default value of none from none option") {
    assert(None.orElse(Some("default")) == Some("default"))
  }

  test("Should be able to get the some option if the filter applies") {
    new Options {
      assert(s1.filter(_ % 2 == 0) == Some(2))
    }
  }

  test("Should be able to get the none option if the filter does not apply") {
    new Options {
      assert(s1.filter(_ % 2 != 0) == None)
    }
  }

  test("Should be able to get a mean of a given sequence in the form of some option") {
    new Options {
      assert(mean(seq) == Some(1.5))
    }
  }

  test("Should be able to a mean of a given sequence in the form of none option") {
    assert(mean(Seq()) == None)
  }

  test("Should be able to calculate variance of a given sequence in the form of some option") {
    new Options {
      assert(variance(seq) == Some(0.25))
    }
  }

  test("Should be able to utilise a function by lifting it to operate in the context of option " +
    "after the fact (pattern matching)") {
    new Options {
      assert(map2(s1, s2)((a, b) => f(a, b)) == Some(7))
    }
  }

  test("Should be able to utilise a function by lifting it to operate in the context of option " +
    "after the fact (pattern matching), passing none as the first parameter and getting none") {
    new Options {
      assert(map2(None, s2)((a, b) => f(a, b)) == None)
    }
  }

  test("Should be able to utilise a function by lifting it to operate in the context of option " +
    "after the fact (pattern matching), passing none as the second parameter and getting none") {
    new Options {
      assert(map2(s1, None)((a, b) => f(a, b)) == None)
    }
  }

  test("Should be able to utilise a function by lifting it to operate in the context of option " +
    "after the fact (pattern matching), passing none parameters") {
    new Options {
      assert(map2(None, None)((a, b) => f(a, b)) == None)
    }
  }

  test("Should be able to utilise a function by lifting it to operate in the context of option " +
    "after the fact (flat map)") {
    new Options {
      assert(map3(s1, s2)((a, b) => f(a, b)) == Some(7))
    }
  }

  test("Should be able to utilise a function by lifting it to operate in the context of option " +
    "after the fact (flat map), passing none as the first parameter and getting none") {
    new Options {
      assert(map3(None, s2)((a, b) => f(a, b)) == None)
    }
  }

  test("Should be able to utilise a function by lifting it to operate in the context of option " +
    "after the fact (flat map), passing none as the second parameter and getting none") {
    new Options {
      assert(map3(s1, None)((a, b) => f(a, b)) == None)
    }
  }

  test("Should be able to utilise a function by lifting it to operate in the context of option " +
    "after the fact (flat map), passing none parameters") {
    new Options {
      assert(map3(None, None)((a, b) => f(a, b)) == None)
    }
  }

  test("Should be able to utilise a function by lifting it to operate in the context of option " +
    "after the fact (for-comprehension)") {
    new Options {
      assert(map4(s1, s2)((a, b) => f(a, b)) == Some(7))
    }
  }

  test("Should be able to utilise a function by lifting it to operate in the context of option " +
    "after the fact (for-comprehension), passing none as the first parameter and getting none") {
    new Options {
      assert(map4(None, s2)((a, b) => f(a, b)) == None)
    }
  }

  test("Should be able to utilise a function by lifting it to operate in the context of option " +
    "after the fact (for-comprehension), passing none as the second parameter and getting none") {
    new Options {
      assert(map4(s1, None)((a, b) => f(a, b)) == None)
    }
  }

  test("Should be able to utilise a function by lifting it to operate in the context of option " +
    "after the fact (for-comprehension), passing none parameters") {
    new Options {
      assert(map4(None, None)((a, b) => f(a, b)) == None)
    }
  }

  test("Should be able to combine a list of some into a some with the list of values (pattern matching)") {
    new Options {
      assert(sequence(l1) == Some(List(2.0, 5.0)))
    }
  }

  test("Should be able to combine a list of a some and a None into a none (pattern matching)") {
    new Options {
      assert(sequence(l2) == None)
    }
  }

  test("Should be able to combine a list of some into a some with the list of values (fold)") {
    new Options {
      assert(sequence_1(l1) == Some(List(2.0, 5.0)))
    }
  }

  test("Should be able to combine a list of a some and a None into a none (fold)") {
    new Options {
      assert(sequence_1(l2) == None)
    }
  }

  test("Should be able to combine a list of some into a some with the list of values (traverse)") {
    new Options {
      assert(sequence_2(l1) == Some(List(2.0, 5.0)))
    }
  }

  test("Should be able to combine a list of a some and a None into a none (traverse)") {
    new Options {
      assert(sequence_2(l2) == None)
    }
  }

  test("Should be able to combine a list of some into a some with the list of values (traverse) " +
    "with a function applied to each value") {
    assert(traverse(List(1, 2, 3))(a => Some(a + 1)) == Some(List(2, 3, 4)))
  }

  test("Should be able to combine an empty list of into a some with an empty list (traverse)") {
    assert(traverse(List())(Some(_)) == Some(List()))
  }

  test("Should be able to combine a list of some into a some with the list of values (folded traverse) " +
    "with a function applied to each value") {
    assert(traverse_1(List(1, 2, 3))(a => Some(a + 1)) == Some(List(2, 3, 4)))
  }

  test("Should be able to combine an empty list of into a some with an empty list (folded traverse)") {
    assert(traverse_1(List())(Some(_)) == Some(List()))
  }
}
