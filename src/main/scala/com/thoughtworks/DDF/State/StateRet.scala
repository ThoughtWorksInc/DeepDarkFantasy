package com.thoughtworks.DDF.State

trait StateRet extends StateBase {
  def stateRet[S <: Type: Kind, A <: Type: Kind]: A ~>: State[S, A]

  final def stateRet_[S <: Type: Kind, A <: Type: Kind](a: A) = app(stateRet[S, A])(a)

  final def stateRet__[S <: Type: Kind, A <: Type: Kind](a: A)(s: S) = app(stateRet_[S, A](a))(s)
}
