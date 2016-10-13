package com.thoughtworks.DDF

import scalaz.{Equal, Monoid}

trait CommutativeMonoid[M] extends Monoid[M] {
  trait CommutativeMonoidLaw extends SemigroupLaw {
    def commutative(a: M, b: M)(implicit F: Equal[M]) = F.equal(append(a, b), append(b, a))
  }
  def commutativeMonoidLaw = new CommutativeMonoidLaw {}
}
