package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.EvalMArr
import com.thoughtworks.DDF.NoInfo
import com.thoughtworks.DDF.Top.EvalMTop

trait EvalMStream extends
  Stream[NoInfo, Lambda[X => X]] with
  SimpleStream[Lambda[X => X]] with
  EvalMTop with
  EvalMArr {
  override def streamNil[A](implicit ai: NoInfo[A]): scala.Stream[A] = Stream.empty

  override def streamCons[A](implicit ai: NoInfo[A]) = a => ls => Stream.cons(a, ls(() : Unit))

  override def streamMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = {
    case Stream.Empty => x => _ => x
    case h #:: t => _ => f => f(h)(t)
  }
}

object EvalMStream extends EvalMStream