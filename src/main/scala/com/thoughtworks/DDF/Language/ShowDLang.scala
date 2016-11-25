package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowDLang extends DLang[NoInfo, Show] with ShowLang {
  override def diff[G:Gradient, A](implicit ai: NoInfo[A]) = Show("diff")
}

object ShowDLang extends ShowDLang