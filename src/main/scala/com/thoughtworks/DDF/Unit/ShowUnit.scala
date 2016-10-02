package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.RI.SimpleRI
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowUnit extends UnitLang[NoInfo, Show] with SimpleUnit[Show] with SimpleRI[Show] {
  override def mkUnit = Show("mkUnit")
}

object ShowUnit {
  implicit def apply = new ShowUnit {}
}