package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.EvalMArr
import com.thoughtworks.DDF.NoInfo

trait EvalMBool extends
  Bool[NoInfo, Lambda[X => X]] with
  SimpleBool[Lambda[X => X]] with
  EvalMArr {
  override def litB: Boolean => Boolean = identity[Boolean]

  override def ite[A](implicit ai: NoInfo[A]): Boolean => A => A => A = {
    case true => x => _ => x
    case false => _ => x => x
  }
}

object EvalMBool extends EvalMBool