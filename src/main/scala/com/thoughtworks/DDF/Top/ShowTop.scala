package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.InfoBase.SimpleInfoBase
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowTop extends
  Top[NoInfo, Lambda[X => Show]] with SimpleTop[Lambda[X => Show]] with SimpleInfoBase[Lambda[X => Show]] {
  override def mkTop = Show("mkTop")
}

object ShowTop extends ShowTop