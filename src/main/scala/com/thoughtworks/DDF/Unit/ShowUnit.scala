package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowUnit extends UnitLang[NoInfo, Show] {
  override def mkUnit = Show("mkUnit")
}
