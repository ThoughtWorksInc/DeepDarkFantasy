package com.thoughtworks.DDF.Cont

import com.thoughtworks.DDF.Arrow.Arr

trait ContRet[Info[_], Repr[_]] extends ContInfo[Info, Repr] with Arr[Info, Repr] {
  def contRet[R: Info, A: Info]: Repr[A => Cont[R, A]]

  final def contRet_[R: Info, A: Info](a: Repr[A]): Repr[Cont[R, A]] =
    app(contRet[R, A])(a)

  final def contRet__[R: Info, A: Info](a: Repr[A])(f: => Repr[A => R]): Repr[R] = app(contRet_[R, A](a))(f)
}
