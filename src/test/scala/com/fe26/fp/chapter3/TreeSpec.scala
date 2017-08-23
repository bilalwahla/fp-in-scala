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
import org.scalatest.Matchers._
import com.fe26.fp.chapter3.Tree._

/**
  * Test specification for `Tree`.
  *
  * @author bilalwahla
  */
class TreeSpec extends FunSpec {
  val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))

  describe("Size") {
    describe("When size of a given tree is calculated") {
      it("Should return correct number of nodes in the given tree using size") {
        Tree.size(t) shouldEqual 9
      }
      it("Should return correct number of nodes in the given tree using size2") {
        Tree.size2(t) shouldEqual 9
      }
    }
  }

  describe("Maximum") {
    describe("When retrieving the maximum in a tree of integers") {
      it("Should return correct maximum integer") {
        maximum(t) shouldEqual 5
      }
      it("Should return correct maximum2 integer") {
        maximum2(t) shouldEqual 5
      }
    }
  }

  describe("Depth") {
    describe("When calculating maximum path length from the root a leaf") {
      it("Should return the maximum length from root to a leaf using depth") {
        depth(t) shouldEqual 3
      }
      it("Should return the maximum length from root to a leaf using depth2") {
        depth2(t) shouldEqual 3
      }
    }
  }

  describe("Map") {
    describe("When adding 1 to each element of a tree of integers") {
      it("Should return the modified tree with elements added with 1 (using map)") {
        map(t)(_ + 1) shouldEqual Branch(Branch(Leaf(2), Leaf(3)), Branch(Branch(Leaf(4), Leaf(5)), Leaf(6)))
      }
      it("Should return the modified tree with elements added with 1 (using map1)") {
        map2(t)(_ + 1) shouldEqual Branch(Branch(Leaf(2), Leaf(3)), Branch(Branch(Leaf(4), Leaf(5)), Leaf(6)))
      }
      it("Should not mutate the original tree") {
        t shouldEqual Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))
      }
    }
  }

  describe("Fold") {
    describe("When calculating a sum of all elements in a tree of integers") {
      val res = fold(t)(a => a + 0)((l, r) => l + r)  // could also write fold(t)(_ + 0)(_ + _)
      it("Should return a correct sum of all elements") {
        res shouldEqual 15
      }
    }
  }
}
