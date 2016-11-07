package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.NoInfo

trait EvalMTop extends
  Top[NoInfo, Lambda[X => X]] with SimpleTop[Lambda[X => X]] {
  override def mkTop: Unit = Unit
}

object EvalMTop extends EvalMTop