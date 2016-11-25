package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowLang extends Lang[NoInfo, Show] with InterLang2Lang[NoInfo, Show] {
  override def i = ShowInterLang
}

object ShowLang extends ShowLang