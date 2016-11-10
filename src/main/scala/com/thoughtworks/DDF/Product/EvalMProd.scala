package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.EvalMArr
import com.thoughtworks.DDF.NoInfo

trait EvalMProd extends
  Prod[NoInfo, Lambda[X => X]] with
  SimpleProd[Lambda[X => X]] with
  EvalMArr {
  override def uncurry[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]): (A => B => C) => ((A, B)) => C =
    f => x => f(x._1)(x._2)

  override def mkProd[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): A => B => (A, B) = l => r => (l, r)

  override def zro[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): ((A, B)) => A = _._1

  override def fst[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): ((A, B)) => B = _._2

  override def curry[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]):
  (((A, B)) => C) => A => B => C = f => a => b => f((a, b))

  override def ><[A, B, C, D](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C], di: NoInfo[D]):
  (A => C) => (B => D) => ((A, B)) => (C, D) = ab => cd => p => (ab(p._1), cd(p._2))
}

object EvalMProd extends EvalMProd