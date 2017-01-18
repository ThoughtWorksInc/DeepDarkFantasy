package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowTop extends Top[NoInfo, Show] with SimpleTop[Show] {
  override def mkTop = Show("mkTop")
}

object ShowTop extends ShowTop