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

  Given("a simple random number generator with a seed of 42")
  val rNG = Simple(42)

  feature("Next integer") {
    scenario("Client generates the next integer") {
      When("the next integer is generated")
      val (n1, rNG2) = rNG.nextInt

      And("the next integer is generated again")
      val (n1Again, _) = rNG.nextInt

      And("next's next integer is generated")
      val (n2, rNG3) = rNG2.nextInt

      And("3rd random integer is generated")
      val ri = int
      val (n3, _) = ri(rNG3)

      Then("next integer is generated successfully")
      assert(n1 == 16159453)

      And("next integer is same every time it is generated")
      assert(n1 == n1Again)

      And("next's next integer is generated successfully")
      assert(n2 == -1281479697)

      And("3rd random integer is generated successfully")
      assert(n3 == -340305902)
    }
  }

  feature("Non-negative integer") {
    scenario("Client generates next non-negative integer") {
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

    scenario("Client generates next double using map") {
      When("generate the next double")
      val (d, _) = randDouble(rNG)

      Then("the next double is generated successfully")
      assert(d == 0.007524831686168909)
    }

    scenario("Client generates next refined double") {
      When("generate the next double")
      val rd = doubleRefined
      val (d, _) = rd(rNG)

      Then("the next double is generated successfully")
      assert(d == 0.007524831686168909)
    }
  }

  feature("Int double") {
    scenario("Client generates next pair of integer double") {
      When("the next pair of integer double is generated")
      val ((int, d), _) = intDouble(rNG)

      Then("the next pair pair of integer double is generated successfully")
      assert(int == 16159453)
      assert(d == 0.5967354848980904)
    }

    scenario("Client generates next pair of integer double using map2") {
      When("the next pair of integer double is generated")
      val ((int1, d1), _) = randIntDouble(rNG)
      val ((int2, d2), _) = randIntDouble2(rNG)

      Then("the next pair pair of integer double is generated successfully")
      assert(int1 == 16159453)
      assert(d1 == 0.5967354848980904)
      assert(int2 == 16159453)
      assert(d2 == 0.5967354848980904)
    }
  }

  feature("Double int") {
    scenario("Client generates next pair of double integer") {
      When("the next pair of double integer is generated")
      val ((d, int), _) = doubleInt(rNG)

      Then("the next pair of integer double is generated successfully")
      assert(d == 0.5967354848980904)
      assert(int == 16159453)
    }

    scenario("Client generates next pair of double integer using map2") {
      When("the next pair of double integer is generated")
      val ((d, int), _) = randDoubleInt(rNG)

      Then("the next pair of integer double is generated successfully")
      assert(d == 0.007524831686168909)
      assert(int == -1281479697)
    }
  }

  feature("Triple double") {
    scenario("Client generates tuple of 3 doubles") {
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
      When("list of 3 random numbers is generated")
      val (iList, _) = ints(3)(rNG)

      Then("a list of 3 random numbers is generated successfully")
      assert(iList == List(16159453, -1281479697, -340305902))
    }

    scenario("Client generates a list of random integers using sequence (combined state actions)") {
      When("list of 3 random numbers is generated")
      val s = _ints(3)
      val (iList, _) = s(rNG)

      Then("a list of 3 random numbers is generated successfully")
      assert(iList == List(16159453, -1281479697, -340305902))
    }
  }

  feature("Unit") {
    scenario("Clients creates a unit") {
      When("a unit is created using a constant value of 2")
      val u = unit(2)

      Then("it should return a `RNG => (2, RNG)` function")
      assert(u.isInstanceOf[Rand[Int]])

      And("passing this function with a random number generator, returns the constant value")
      val (c, rNG2) = u(rNG)
      assert(c == 2)
      assert(rNG2 == rNG)
    }
  }

  feature("Map") {
    scenario("Client transforms the output of a state action") {
      When("map is used to transform the output of a state action to generate an `Int` that’s " +
        "greater than or equal to zero and divisible by two")
      /*
      Here we could use any of our earlier implementations that essentially are `RNG => (2, RNG)`
       */
      val m1 = map(nonNegativeInt)(a => a - a % 2)
      val m2 = mapUsingFlatMap(nonNegativeInt)(a => a - a % 2)

      Then("it should return a `RNG => (nonNegativeInt, RNG)` function")
      assert(m1.isInstanceOf[Rand[Int]])

      And("passing this function with a random number generator, returns a positive even integer")
      val (i1, _) = m1(rNG)
      assert(i1 == 16159452)
      val (i2, _) = m2(rNG)
      assert(i2 == 16159452)
    }
  }

  feature("Non-negative less than") {
    Given("an n of 500 and simple random number generator with a seed of 0 and another n of -1")
    val n1 = 500
    val rNG2 = Simple(0)
    val n2 = -1

    scenario("Client generates a non-negative integer less than n using map") {
      When("a non-negative integer less than n is generated")
      val m = nonNegativeLessThanSkewed(n1)
      val(int, _) = m(rNG)

      Then("a skewed non-negative integer less than n is generated successfully")
      assert(int >= 0 && int < n1)
    }

    scenario("Client generates a non-negative integer less than n using an explicit implementation") {
      When("a non-negative integer less than n is generated")
      val m1 = nonNegativeLessThan(n1)
      val(int, _) = m1(rNG)
      val m2 = nonNegativeLessThan(n2)
      val(int2, _) = m2(rNG2)

      Then("a non-negative integer less than n is generated successfully")
      assert(int >= 0 && int < n1)
      assert(int2 >= 0 && int2 < n1)
    }

    scenario("Client generates a non-negative integer less than n using flat map") {
      When("a non-negative integer less than n is generated")
      val m = nonNegativeLessThanFlatMap(n1)
      val(int, _) = m(rNG)
      val m2 = nonNegativeLessThanFlatMap(n2)
      val(int2, _) = m2(rNG2)

      Then("a non-negative integer less than n is generated successfully")
      assert(int >= 0 && int < n1)
      assert(int2 >= 0 && int2 < n1)
    }
  }
}
