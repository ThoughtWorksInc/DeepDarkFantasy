package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait App extends Arr {
  def App[A <: Type: Kind, B <: Type: Kind]: (A ~>: B) ~>: A ~>: B

  final def App_[A <: Type: Kind, B <: Type: Kind](f: A ~>: B) = app(App[A, B])(f)

  final def App__[A <: Type: Kind, B <: Type: Kind](f: A ~>: B)(a: A) = app(App_(f))(a)
}
