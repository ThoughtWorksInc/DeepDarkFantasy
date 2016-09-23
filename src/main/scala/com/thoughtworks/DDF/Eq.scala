package com.thoughtworks.DDF

import scalaz.Leibniz._

object Eq {

  def PairEq[A, B, C, D]: A === C => B === D => (A, B) === (C, D) = ac => bd =>
    ac.subst[Lambda[C => (A, B) === (C, D)]](bd.subst[Lambda[D => (A, B) === (A, D)]](refl[(A, B)]))

  def PairFstEq[A, B, C, D]: (A, B) === (C, D) => A === C = _ => force[Nothing, Any, A, C]

  def PairSndEq[A, B, C, D]: (A, B) === (C, D) => B === D = _ => force[Nothing, Any, B, D]

  def ArrEq[A, B, C, D]: A === C => B === D => (A => B) === (C => D) = dom => rng =>
    dom.subst[Lambda[X => (A => B) === (X => D)]](rng.subst[Lambda[X => (A => B) === (A => X)]](refl[A => B]))

  def ArrDomEq[A, B, C, D]: (A => B) === (C => D) => A === C = _ => force[Nothing, Any, A, C]

  def ArrRngEq[A, B, C, D]: (A => B) === (C => D) => B === D = _ => force[Nothing, Any, B, D]

}
