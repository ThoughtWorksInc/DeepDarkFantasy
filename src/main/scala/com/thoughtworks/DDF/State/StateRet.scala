package com.thoughtworks.DDF.State

trait StateRet[Info[_], Repr[_]] extends StateBase[Info, Repr] {
  def stateRet[S, A](implicit si: Info[S], ai: Info[A]): Repr[A => State[S, A]]

  final def stateRet_[S, A](a: Repr[A])(implicit si: Info[S]): Repr[State[S, A]] = app(stateRet(si, reprInfo(a)))(a)

  final def stateRet__[S, A]: Repr[A] => Repr[S] => Repr[(A, S)] = a => s => app(stateRet_(a)(reprInfo(s)))(s)
}
