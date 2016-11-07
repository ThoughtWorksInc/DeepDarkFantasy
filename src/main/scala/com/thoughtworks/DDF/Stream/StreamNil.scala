package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.Arr

trait StreamNil[Info[_], Repr[_]] extends StreamInfo[Info, Repr] with Arr[Info, Repr] {
  def streamNil[A](implicit ai: Info[A]): Repr[scala.Stream[A]]
}
