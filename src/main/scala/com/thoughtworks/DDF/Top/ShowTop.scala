package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.LangBase.SimpleLangBase
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowTop extends
  Top[NoInfo, Lambda[X => Show]] with SimpleTop[Lambda[X => Show]] with SimpleLangBase[Lambda[X => Show]] {
  override def mkTop = Show("mkTop")
}

object ShowTop extends ShowTop