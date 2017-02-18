package com.thoughtworks.DDF.State

trait StateBind extends StateBase {
  def stateBind[S, A, B](implicit si: Info[S], ai: Info[A], bi: Info[B]):
  Repr[State[S, A] => (A => State[S, B]) => State[S, B]]

  final def stateBind_[S, A, B](s: Repr[State[S, A]])(implicit bi: Info[B]): Repr[(A => State[S, B]) => State[S, B]] =
    app(stateBind(stateInfoS(reprInfo(s)), stateInfoA(reprInfo(s)), bi))(s)

  final def stateBind__[S, A, B]: Repr[State[S, A]] => Repr[A => State[S, B]] => Repr[State[S, B]] = s => f =>
    app(stateBind_[S, A, B](s)(stateInfoA(rngInfo(reprInfo(f)))))(f)

  final def stateBind___[S, A, B]: Repr[State[S, A]] => Repr[A => State[S, B]] => Repr[S] => Repr[(B, S)] = s => f =>
    app(stateBind__(s)(f))
}
