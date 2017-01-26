package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.{NoInfo, ShowLeaf}

trait ShowLang extends Lang[NoInfo, ShowLeaf] with InterLang2Lang[NoInfo, ShowLeaf] {
  override def i = ShowInterLang
}

object ShowLang extends ShowLang