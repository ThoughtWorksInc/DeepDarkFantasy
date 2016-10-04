package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.EvalArrow
import com.thoughtworks.DDF.{Eval, Loss}

trait EvalOption extends OptionRepr[Loss, Eval] with EvalArrow {
  override implicit def optionInfo[A](implicit ai: Loss[A]): Loss[Option[A]] = ???

  override def optionElmInfo[A]: (Loss[Option[A]]) => Loss[A] = ???

  override def none[A](implicit ai: Loss[A]): Eval[Option[A]] = ???

  override def some[A](implicit ai: Loss[A]): Eval[(A) => Option[A]] = ???

  override def optionMatch[A, B](implicit ai: Loss[A], bi: Loss[B]): Eval[(B) => ((A) => B) => (Option[A]) => B] = ???
}
