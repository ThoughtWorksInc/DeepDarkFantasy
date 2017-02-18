package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.Arr
import com.thoughtworks.DDF.Top.Top

trait StreamCons extends StreamType with Top with Arr {
  def streamCons[A <: Type: Kind]: A ~>: (Top ~>: Stream[A]) ~>: scala.Stream[A]

  final def streamCons_[A <: Type: Kind](a: A) = app(streamCons[A])(a)

  final def streamCons__[A <: Type: Kind](a: A)(s: Unit => scala.Stream[A]) = app(streamCons_(a))(s)
}
