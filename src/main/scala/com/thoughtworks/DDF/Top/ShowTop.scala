package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.InfoBase.SimpleInfoBase
import com.thoughtworks.DDF.{NoInfo, ShowLeaf}

trait ShowTop extends Top[NoInfo, ShowLeaf] with SimpleTop[ShowLeaf] with SimpleInfoBase[ShowLeaf] {
  override def mkTop = ShowLeaf("mkTop")
}

object ShowTop extends ShowTop