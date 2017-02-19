package com.thoughtworks.DDF.Cont

import com.thoughtworks.DDF.Arrow.Arr

trait ContRet extends ContType with Arr {
  def contRet[R, A]: A ~>: Cont[R, A]

  final def contRet_[R, A](a: A): Cont[R, A] = app(contRet[R, A])(a)

  final def contRet__[R, A](a: A)(k: A ~>: R) = app(contRet_[R, A](a))(k)
}
