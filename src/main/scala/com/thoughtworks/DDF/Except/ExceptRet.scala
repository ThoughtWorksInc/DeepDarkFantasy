package com.thoughtworks.DDF.Except

trait ExceptRet extends ExceptBase {
  def exceptRet[A <: Type: Kind, B <: Type: Kind]: B ~>: Except[A, B] = right[A, B]

  final def exceptRet_[A <: Type: Kind, B <: Type: Kind](b: B): Except[A, B] = app(exceptRet[A, B])(b)
}
