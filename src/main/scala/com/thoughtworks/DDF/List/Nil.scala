package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.Arr

trait Nil extends ListType with Arr {
  def nil[A <: Type: Kind]: List[A]
}
