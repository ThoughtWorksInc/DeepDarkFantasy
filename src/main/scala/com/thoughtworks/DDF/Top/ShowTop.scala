package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.InfoBase.SimpleInfoBase
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowTop extends Top[NoInfo, Show] with SimpleTop[Show] with SimpleInfoBase[Show] {
  override def mkTop = Show("mkUnit")
}

object ShowTop {
  implicit def apply = new ShowTop {}
}