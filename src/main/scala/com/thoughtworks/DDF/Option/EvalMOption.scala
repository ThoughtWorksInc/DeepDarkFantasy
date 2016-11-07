package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.EvalMArr
import com.thoughtworks.DDF.NoInfo

trait EvalMOption extends
  Option[NoInfo, Lambda[X => X]] with
  SimpleOption[Lambda[X => X]] with
  EvalMArr {
  override def none[A](implicit ai: NoInfo[A]): scala.Option[A] = None

  override def some[A](implicit ai: NoInfo[A]): A => scala.Option[A] = Some[A]

  override def optionMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): scala.Option[A] => B => (A => B) => B = {
    case None => x => _ => x
    case Some(x) => _ => f => f(x)
  }
}

object EvalMOption extends EvalMOption