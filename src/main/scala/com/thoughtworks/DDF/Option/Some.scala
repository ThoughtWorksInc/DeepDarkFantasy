package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.Arr

trait Some extends OptionType with Arr {
  def some[A <: Type: Kind]: A ~>: Option[A]

  final def some_[A <: Type: Kind](a: A) = app(some[A])(a)
}
