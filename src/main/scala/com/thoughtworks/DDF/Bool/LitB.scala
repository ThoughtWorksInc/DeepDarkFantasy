package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.Arr

trait LitB extends Arr with BoolType {
  def litB: Boolean => Bool
}
