package com.thoughtworks.DDF.Cont

import com.thoughtworks.DDF.Arrow.Arr

trait ContBind extends ContType with Arr {
  def contBind[R, A, B](implicit ri: Info[R], ai: Info[A], bi: Info[B]):
  Repr[Cont[R, A] => (A => Cont[R, B]) => Cont[R, B]]

  final def contBind_[R, A, B](ma: Repr[Cont[R, A]])(implicit bi: Info[B]): Repr[(A => Cont[R, B]) => Cont[R, B]] =
    app(contBind(contInfoR(reprInfo(ma)), contInfoA(reprInfo(ma)), bi))(ma)

  final def contBind__[R, A, B]: Repr[Cont[R, A]] => Repr[A => Cont[R, B]] => Repr[Cont[R, B]] = ma => f =>
    app(contBind_(ma)(contInfoA(rngInfo(reprInfo(f)))))(f)

  final def contBind___[R, A, B]: Repr[Cont[R, A]] => Repr[A => Cont[R, B]] => Repr[B => R] => Repr[R] = ma => f =>
    app(contBind__(ma)(f))
}
