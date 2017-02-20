package com.thoughtworks.DDF.Reader

import com.thoughtworks.DDF.Arrow.Arr

trait ReaderBase extends Arr {
  final type Reader[-E <: Type, +T <: Type] = E ~>: T
}
