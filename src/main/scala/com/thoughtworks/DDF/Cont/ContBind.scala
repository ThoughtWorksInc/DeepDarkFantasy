package com.thoughtworks.DDF.Cont

import com.thoughtworks.DDF.Arrow.Arr

trait ContBind extends ContType with Arr {
  def contBind[R <: Type: Kind, A <: Type: Kind, B <: Type: Kind]: Cont[R, A] ~>: (A ~>: Cont[R, B]) ~>: Cont[R, B]

  final def contBind_[R <: Type: Kind, A <: Type: Kind, B <: Type: Kind](ma: Cont[R, A]) = app(contBind[R, A, B])(ma)

  final def contBind__[R <: Type: Kind, A <: Type: Kind, B <: Type: Kind](ma: Cont[R, A])(f: A ~>: Cont[R, B]) =
    app(contBind_[R, A, B](ma))(f)

  final def contBind___[R <: Type: Kind, A <: Type: Kind, B <: Type: Kind]
  (ma: Cont[R, A])(f: A ~>: Cont[R, B])(k: B ~>: R) = app(contBind__(ma)(f))(k)
}
