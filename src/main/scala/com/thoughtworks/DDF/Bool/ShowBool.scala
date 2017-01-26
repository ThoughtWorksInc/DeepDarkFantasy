package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowBool extends Bool[NoInfo, Lambda[X => Show]] with SimpleBool[Lambda[X => Show]] with ShowArr {
  override def litB = b => Show(b.toString)

  override def ite[A](implicit ai: NoInfo[A]) = Show("ite")
}

object ShowBool extends ShowBool