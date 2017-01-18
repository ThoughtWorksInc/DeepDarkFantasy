package com.thoughtworks.DDF.State

trait StateRet[Info[_], Repr[_]] extends StateBase[Info, Repr] {
  def stateRet[S: Info, A: Info]: Repr[A => State[S, A]]

  final def stateRet_[S: Info, A: Info](a: Repr[A]): Repr[State[S, A]] = app(stateRet[S, A])(a)

  final def stateRet__[S: Info, A: Info](a: Repr[A])(s: Repr[S]): Repr[(A, S)] = app(stateRet_[S, A](a))(s)
}
