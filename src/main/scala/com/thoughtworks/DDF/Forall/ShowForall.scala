package com.thoughtworks.DDF.Forall

import com.thoughtworks.DDF.Arrow.ShowArrow
import com.thoughtworks.DDF.{NoInfo, Show}

import scalaz.{Forall, NaturalTransformation}

trait ShowForall extends SimpleForall[Show] with ShowArrow {
  override def specialize[F[_], A](implicit ai: NoInfo[A], fi: FInfo[F]) = Show("specialize")
}

object ShowForall {
  implicit def apply = new ShowForall {}
}