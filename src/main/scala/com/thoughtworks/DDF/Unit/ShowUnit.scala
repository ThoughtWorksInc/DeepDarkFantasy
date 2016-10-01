package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowUnit extends UnitLang[NoInfo, Show] with SimpleUnit[Show] {
  override def mkUnit = Show("mkUnit")
}

object ShowUnit {
  implicit def apply = new ShowUnit {}
}