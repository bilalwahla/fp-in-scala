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

package com.fe26.fp.chapter6

import com.fe26.fp.chapter6.RNG.{Simple, double, int, nonNegativeInt}
import org.scalatest.{FeatureSpec, GivenWhenThen}

/**
  * Test specification for State.
  *
  * @author bilalwahla
  */
class StateSpec extends FeatureSpec with GivenWhenThen {
  Given("a simple random number generator with a seed of 42")
  val rNG = Simple(42)

  feature("State") {
    scenario("Map - Client transforms the output of a state action") {
      When("map is used to transform the output of a state action to generate an `Int` that’s " +
        "greater than or equal to zero and divisible by two")
      val s = State(nonNegativeInt).map(a => a - a % 2)

      Then("running this state with a random number generator, returns a positive even integer")
      val (i, _) = s.run(rNG)
      assert(i == 16159452)
    }

    scenario("Map 2 - Client generates the next random integer and a double") {
      When("the next random integer and a double is prepared to be generated")
      val s = State(int).map2(State(double))((_, _))

      Then("running this state generates the next random integer and a double")
      val ((i, d), _) = s.run(rNG)
      assert(i == 16159453)
      assert(d == 0.5967354848980904)
    }
  }
}
