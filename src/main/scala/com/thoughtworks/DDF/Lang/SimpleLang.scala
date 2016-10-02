package com.thoughtworks.DDF.Lang

import com.thoughtworks.DDF.List.SimpleList
import com.thoughtworks.DDF.NoInfo

trait SimpleLang[Repr[_]] extends
  Lang[NoInfo, Repr] with
  SimpleList[Repr]