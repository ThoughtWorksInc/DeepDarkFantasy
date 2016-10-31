package com.thoughtworks.DDF.Cont

trait ContBind[Info[_], Repr[_]] extends ContBase[Info, Repr] {
  def contBind[R, A, B](implicit ri: Info[R], ai: Info[A], bi: Info[B]):
  Repr[Cont[R, A] => (A => Cont[R, B]) => Cont[R, B]]

  final def contBind_[R, A, B](ma: Repr[Cont[R, A]])(implicit bi: Info[B]): Repr[(A => Cont[R, B]) => Cont[R, B]] =
    app(contBind(rngInfo(reprInfo(ma)), domInfo(domInfo(reprInfo(ma))), bi))(ma)

  final def contBind__[R, A, B]: Repr[Cont[R, A]] => Repr[A => Cont[R, B]] => Repr[Cont[R, B]] = ma => f =>
    app(contBind_(ma)(domInfo(domInfo(rngInfo(reprInfo(f))))))(f)

  final def contBind___[R, A, B]: Repr[Cont[R, A]] => Repr[A => Cont[R, B]] => Repr[B => R] => Repr[R] = ma => f =>
    app(contBind__(ma)(f))
}
