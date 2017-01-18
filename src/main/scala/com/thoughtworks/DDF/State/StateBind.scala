package com.thoughtworks.DDF.State

trait StateBind[Info[_], Repr[_]] extends StateBase[Info, Repr] {
  def stateBind[S: Info, A: Info, B: Info]: Repr[State[S, A] => (A => State[S, B]) => State[S, B]]

  final def stateBind_[S: Info, A: Info, B: Info](st: Repr[State[S, A]]): Repr[(A => State[S, B]) => State[S, B]] =
    app(stateBind[S, A, B])(st)

  final def stateBind__[S: Info, A: Info, B: Info](st: Repr[State[S, A]])(f: Repr[A => State[S, B]]): Repr[State[S, B]] =
    app(stateBind_[S, A, B](st))(f)

  final def stateBind___[S: Info, A: Info, B: Info](st: Repr[State[S, A]])(f: Repr[A => State[S, B]])(s: Repr[S]):
  Repr[(B, S)] = app(stateBind__(st)(f))(s)
}
