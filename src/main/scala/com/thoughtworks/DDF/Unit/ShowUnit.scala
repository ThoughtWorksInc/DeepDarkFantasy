package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.InfoBase.SimpleInfoBase
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowUnit extends Unit[NoInfo, Show] with SimpleUnit[Show] with SimpleInfoBase[Show] {
  override def mkUnit = Show("mkUnit")
}

object ShowUnit {
  implicit def apply = new ShowUnit {}
}