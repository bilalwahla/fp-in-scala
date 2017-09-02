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

  feature("Next double") {
    scenario("Client generates next double") {
      Given("a simple random number generator with seed 42")
      val rNG = Simple(42)

      When("generate the next double")
      val (d1, rNG2) = double(rNG)

      And("generate next's next double")
      val (d2, _) = double(rNG2)

      Then("the next double is generated successfully")
      assert(d1 == 0.007524831686168909)

      And("the next's next double is generated successfully")
      assert(d2 == 0.5967354848980904)

      And("the next and the next's next double are between 0 and 1")
      assert(d1 < 1 && d2 < 1)
    }
  }

  feature("Int double") {
    scenario("Client generates next pair of integer double") {
      Given("a simple random number generator with seed 42")
      val rNG = Simple(42)

      When("the next pair of integer double is generated")
      val ((int, d), _) = intDouble(rNG)

      Then("the next pair pair of integer double is generated successfully")
      assert(int == 16159453)
      assert(d == 0.5967354848980904)
    }
  }

  feature("Double int") {
    scenario("Client generates next pair of double integer") {
      Given("a simple random number generator with seed 42")
      val rNG = Simple(42)

      When("the next pair of double integer is generated")
      val ((d, int), _) = doubleInt(rNG)

      Then("the next pair of integer double is generated successfully")
      assert(d == 0.5967354848980904)
      assert(int == 16159453)
    }
  }

  feature("Triple double") {
    scenario("Client generates tuple of 3 doubles") {
      Given("a simple random number generator with seed 42")
      val rNG = Simple(42)

      When("the next tuple of 3 doubles is generated")
      val ((d1, d2, d3), _) = double3(rNG)

      Then("the next tuple of 3 doubles is generated successfully")
      assert(d1 == 0.007524831686168909)
      assert(d2 == 0.5967354848980904)
      assert(d3 == 0.15846728393808007)
    }
  }

  feature("List of random integers") {
    scenario("Client generates a list of random integers") {
      Given("a simple random number generator with seed 42")
      val rNG = Simple(42)

      When("list of 3 random numbers is generated")
      val (iList, _) = ints(3)(rNG)

      Then("a list of 3 random numbers is generated successfully")
      assert(iList == List(16159453, -1281479697, -340305902))
    }
  }
}
