package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowLang extends Lang[NoInfo, Lambda[X => Show]] with InterLang2Lang[NoInfo, Lambda[X => Show]] {
  override def i = ShowInterLang
}

object ShowLang extends ShowLang