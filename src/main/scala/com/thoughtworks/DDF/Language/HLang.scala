package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Hoas.Hoas

trait HLang[Info[_], Repr[_]] extends
  Lang[Info, Repr] with
  Hoas[Info, Repr]
