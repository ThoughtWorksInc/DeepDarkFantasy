package com.thoughtworks.DDF.State

trait StateBind extends StateBase {
  def stateBind[S <: Type: Kind, A <: Type: Kind, B <: Type: Kind]: State[S, A] ~>: (A ~>: State[S, B]) ~>: State[S, B]

  final def stateBind_[S <: Type: Kind, A <: Type: Kind, B <: Type: Kind](st: State[S, A]) = app(stateBind[S, A, B])(st)

  final def stateBind__[S <: Type: Kind, A <: Type: Kind, B <: Type: Kind](st: State[S, A])(f: A ~>: State[S, B]) =
    app(stateBind_[S, A, B](st))(f)

  final def stateBind___[S <: Type: Kind, A <: Type: Kind, B <: Type: Kind]
  (st: State[S, A])(f: A ~>: State[S, B])(s: S) = app(stateBind__[S, A, B](st)(f))(s)
}
