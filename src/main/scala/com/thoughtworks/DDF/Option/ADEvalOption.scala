package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.ADEvalArr
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.{ADEval, ADEvalCase, ADEvalMatch}

trait ADEvalOption extends Option[ADEvalCase, ADEval] with ADEvalArr {
  override def none[A](implicit ai: ADEvalCase[A]) =
    new ADEval[scala.Option[A]] {
      override val fec = optionInfo(ai)

      override def term[G: Gradient] = base.none(ai.wgi[G])
    }

  override def some[A](implicit ai: ADEvalCase[A]) =
    new ADEval[A => scala.Option[A]] {
      override val fec = aInfo(ai, optionInfo(ai))

      override def term[G: Gradient] = base.some(ai.wgi[G])
    }

  override def optionMatch[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[scala.Option[A] => B => (A => B) => B] {
      override val fec = aInfo(optionInfo(ai), aInfo(bi, aInfo(aInfo(ai, bi), bi)))

      override def term[G: Gradient] = base.optionMatch(ai.wgi[G], bi.wgi[G])
    }

  override implicit def optionInfo[A](implicit ai: ADEvalCase[A]):
  ADEvalCase.Aux[scala.Option[A], Lambda[G => scala.Option[ai.WithGrad[G]]]] =
    new ADEvalCase[scala.Option[A]] {
      override type WithGrad[G] = scala.Option[ai.WithGrad[G]]

      override val tm = ofem[A]

      override def tmr: tm.ret = ai

      override def wgi[G: Gradient] = base.optionInfo(ai.wgi[G])
    }

  override def optionElmInfo[A]: ADEvalCase[scala.Option[A]] => ADEvalCase[A] = _.get(ofem[A])

  def ofem[A] = new ADEvalMatch[scala.Option[A]] {
    override type ret = ADEvalCase[A]
  }
}

object ADEvalOption extends ADEvalOption