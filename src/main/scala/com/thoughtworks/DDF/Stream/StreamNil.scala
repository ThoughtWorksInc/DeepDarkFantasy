package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.Arr

trait StreamNil extends StreamType with Arr {
  def streamNil[A <: Type: Kind]: Stream[A]
}
