package com.fe26.fp.chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /* Exercise 3.25 */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => 1 + l + r)

  /* Exercise 3.26 */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def maximum2(t: Tree[Int]): Int = fold(t)(a => a)((l, r) => l max r)

  /* Exercise 3.27 */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def depth2[A](t: Tree[A]): Int = fold(t)(_ => 0)((l, r) => 1 + (l max r))

  /* Exercise 3.28 */
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))

  /**
    * Exercise 3.29
    *
    * @param t tree to fold
    * @param f function to apply on leaves
    * @param g function to be applied on branches
    * @tparam A type of elements in the tree
    * @tparam B type of the produced result
    * @return result of applying the given functions on nodes of the tree
    */
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}