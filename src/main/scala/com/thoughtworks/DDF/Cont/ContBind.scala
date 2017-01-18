package com.thoughtworks.DDF.Cont

import com.thoughtworks.DDF.Arrow.Arr

trait ContBind[Info[_], Repr[_]] extends ContInfo[Info, Repr] with Arr[Info, Repr] {
  def contBind[R: Info, A: Info, B: Info]: Repr[Cont[R, A] => (A => Cont[R, B]) => Cont[R, B]]

  final def contBind_[R: Info, A: Info, B: Info](ma: Repr[Cont[R, A]]): Repr[(A => Cont[R, B]) => Cont[R, B]] =
    app(contBind[R, A, B])(ma)

  final def contBind__[R: Info, A: Info, B: Info](ma: Repr[Cont[R, A]])(f: Repr[A => Cont[R, B]]): Repr[Cont[R, B]] =
    app(contBind_[R, A, B](ma))(f)

  final def contBind___[R: Info, A: Info, B: Info](ma: Repr[Cont[R, A]])(f: Repr[A => Cont[R, B]])(x: Repr[B => R]):
  Repr[R] = app(contBind__(ma)(f))(x)
}
