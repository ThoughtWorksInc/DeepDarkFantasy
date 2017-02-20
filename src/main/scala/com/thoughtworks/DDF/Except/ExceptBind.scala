package com.thoughtworks.DDF.Except

import com.thoughtworks.DDF.Arrow.Arr

trait ExceptBind extends ExceptBase with Arr {
  def exceptBind[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind]:
  Except[A, B] ~>: (B ~>: Except[A, C]) ~>: Except[A, C]

  final def exceptBind_[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](e: Except[A, B]) =
    app(exceptBind[A, B, C])(e)

  final def exceptBind__[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](e: Except[A, B])(f: B ~>: Except[A, C]) =
    app(exceptBind_[A, B, C](e))(f)
}
