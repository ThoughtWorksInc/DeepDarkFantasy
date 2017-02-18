package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.Arr

trait Cons extends ListType with Arr {
  def cons[A <: Type: Kind]: A ~>: List[A] ~>: List[A]

  final def cons_[A <: Type: Kind](a: A): List[A] ~>: List[A] = app(cons)(a)

  final def cons__[A <: Type: Kind](a: A)(l: List[A]): List[A] = app(cons_(a))(l)
}
