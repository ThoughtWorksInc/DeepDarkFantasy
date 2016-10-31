package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.NoInfo

trait EvalMArr extends Arr[NoInfo, Lambda[X => X]] with SimpleArr[Lambda[X => X]] {
  override def app[A, B]: (A => B) => A => B = identity[A => B]
}

object EvalMArr extends EvalMArr