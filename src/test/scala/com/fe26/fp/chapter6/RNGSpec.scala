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
import com.fe26.fp.chapter6.RNG._

/**
  * RNG test specification.
  *
  * NOTE: FeatureSpec is primarily intended for acceptance testing, including facilitating the
  * process of programmers working alongside non-programmers to define the acceptance requirements.
  *
  * @author bilalwahla
  */
class RNGSpec extends FeatureSpec with GivenWhenThen {
  info("As a client of RNG API")
  info("I want to be able to generate random numbers")
  info("In a way that transition from one state to the next is explicit")
  info("Subsequently making it a purely functional API")

  feature("Next integer") {
    scenario("Client generates the next integer") {
      Given("a simple random number generator with a seed of 42")
      val rNG = Simple(42)

      When("the next integer is generated")
      val (n1, rNG2) = rNG.nextInt

      And("the next integer is generated again")
      val (n1Again, _) = rNG.nextInt

      And("next's next integer is generated")
      val (n2, _) = rNG2.nextInt

      Then("next integer is generated successfully")
      assert(n1 == 16159453)

      And("next integer is same every time it is generated")
      assert(n1 == n1Again)

      And("next's next integer is generated successfully")
      assert(n2 == -1281479697)
    }
  }

  feature("Non-negative integer") {
    scenario("Client generates next non-negative integer") {
      Given("a simple random number generator with seed 42")
      val rNG = Simple(42)

      When("next non-negative integer is generated")
      val (n1, rNG2) = nonNegativeInt(rNG)

      And("next's next non-negative integer is generated")
      val (n2, _) = nonNegativeInt(rNG2)

      Then("a non-negative integer is generated successfully")
      assert(n1 >= 0 && n2 >= 0)
    }
  }
}
