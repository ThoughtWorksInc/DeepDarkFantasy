package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Diff.Diff

trait DLang[Info[_], Repr[_]] extends
  Lang[Info, Repr] with
  Diff[Info, Repr]
