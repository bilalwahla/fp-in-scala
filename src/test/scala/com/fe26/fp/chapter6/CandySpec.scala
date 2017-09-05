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

import org.scalatest.{FeatureSpec, GivenWhenThen}

/**
  * Test Specification for Candy.
  *
  * @author bilalwahla
  */
class CandySpec extends FeatureSpec with GivenWhenThen {
  feature("Automaton - Candy dispenser") {
    scenario("Inserting a coin into a locked machine with a candy left, will unlock it") {
      Given("a locked machine with a candy left")
      val m = Machine(locked = true, 1, 0)

      When("a coin is inserted into this locked machine")
      val ((coins, candies), m2) = Candy.simulateMachine(List(Coin)).run(m)

      Then("machine will unlock")
      assert(candies == 1)
      assert(coins == 1)
      assert(!m2.locked)
    }

    scenario("Turning knob on an unlocked machine will make it dispense candy and become locked") {
      Given("an unlocked machine")
      val m = Machine(locked = false, 1, 1)

      When("the knob on the unlocked machine is turned")
      val ((coins, candies), m2) = Candy.simulateMachine(List(Turn)).run(m)

      Then("machine will dispense candy")
      assert(candies == 0)
      assert(coins == 1)
      assert(m2.locked)
    }

    scenario("Turning knob on a locked machine does nothing") {
      Given("an locked machine")
      val m = Machine(locked = true, 11, 0)

      When("the knob on the locked machine is turned")
      val ((coins, candies), m2) = Candy.simulateMachine(List(Turn)).run(m)

      Then("locked machine will does nothing")
      assert(candies == 11)
      assert(coins == 0)
      assert(m2.locked)
    }

    scenario("Inserting a coin into an unlocked machine does nothing") {
      Given("an unlocked machine")
      val m = Machine(locked = false, 11, 1)

      When("the knob on the locked machine is turned")
      val ((coins, candies), m2) = Candy.simulateMachine(List(Coin)).run(m)

      Then("already unlocked machine will does nothing")
      assert(candies == 11)
      assert(coins == 1)
      assert(!m2.locked)
    }

    scenario("A machine that’s out of candy ignores all inputs") {
      Given("a locked out of candy machine")
      val m = Machine(locked = true, 0, 12)

      When("a coin is inserted into this locked machine")
      val ((coins1, candies1), m2) = Candy.simulateMachine(List(Coin)).run(m)

      And("the knob on the locked machine is turned")
      val ((coins2, candies2), m3) = Candy.simulateMachine(List(Turn)).run(m)

      Then("out of candy machine ignores all inputs")
      assert(coins1 == 12)
      assert(candies1 == 0)
      assert(m2.locked)
      assert(coins2 == 12)
      assert(candies2 == 0)
      assert(m3.locked)
    }
  }
}
