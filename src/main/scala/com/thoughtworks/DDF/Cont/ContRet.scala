package com.thoughtworks.DDF.Cont

import com.thoughtworks.DDF.Arrow.Arr

trait ContRet extends ContType with Arr {
  def contRet[R <: Type: Kind, A <: Type: Kind]: A ~>: Cont[R, A]

  final def contRet_[R <: Type: Kind, A <: Type: Kind](a: A): Cont[R, A] = app(contRet[R, A])(a)

  final def contRet__[R <: Type: Kind, A <: Type: Kind](a: A)(k: A ~>: R) = app(contRet_[R, A](a))(k)
}
