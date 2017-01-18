package com.thoughtworks.DDF.Cont

import com.thoughtworks.DDF.Arrow.Arr

trait ContRet[Info[_], Repr[_]] extends ContInfo[Info, Repr] with Arr[Info, Repr] {
  def contRet[R, A](implicit ri: Info[R], ai: Info[A]): Repr[A => Cont[R, A]]

  final def contRet_[R, A](a: Repr[A])(implicit ri: Info[R]): Repr[Cont[R, A]] =
    app(contRet(ri, reprInfo(a)))(a)

  final def contRet__[R, A]: Repr[A] => Repr[A => R] => Repr[R] = a => f =>
    app(contRet_(a)(rngInfo(reprInfo(f))))(f)
}
