package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.EvalMArr
import com.thoughtworks.DDF.NoInfo

trait EvalMComb extends Comb[NoInfo, Lambda[X => X]] with EvalMArr {
  override def S[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]):
  (A => B => C) => (A => B) => A => C = f => x => a => f(a)(x(a))

  override def Y[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): ((A => B) => A => B) => A => B = f => x =>
    f(Y[A, B](NoInfo(), NoInfo())(f))(x)

  override def Let[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): A => (A => B) => B = x => f => f(x)

  override def W[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): (A => A => B) => A => B = f => a => f(a)(a)

  override def K[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): A => B => A = a => _ => a

  override def App[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): (A => B) => A => B = identity[A => B]

  override def C[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]): (A => B => C) => B => A => C = f =>
    y => x => f(x)(y)

  override def I[A](implicit ai: NoInfo[A]): A => A = identity[A]

  override def B[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]): (B => C) => (A => B) => A => C =
    f => g => f.compose(g)
}

object EvalMComb extends EvalMComb