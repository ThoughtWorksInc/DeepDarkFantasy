package com.thoughtworks.DDF.List

trait Reverse extends ListMin {
  def reverse[A <: Type: Kind]: List[A] ~>: List[A]

  final def reverse_[A <: Type: Kind](l: List[A]): List[A] = app(reverse)(l)
}
