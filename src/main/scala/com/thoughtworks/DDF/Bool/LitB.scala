package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.Arr

trait LitB[Info[_], Repr[_]] extends Arr[Info, Repr] with BoolInfo[Info, Repr] {
  def litB: Boolean => Repr[Boolean]
}
