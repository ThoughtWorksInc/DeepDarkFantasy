package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.InfoB.SimpleInfoB
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowUnit extends UnitRepr[NoInfo, Show] with SimpleUnit[Show] with SimpleInfoB[Show] {
  override def mkUnit = Show("mkUnit")
}

object ShowUnit {
  implicit def apply = new ShowUnit {}
}