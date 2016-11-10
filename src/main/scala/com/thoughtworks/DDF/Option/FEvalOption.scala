package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.FEvalArr
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.{FEval, FEvalCase, FEvalMatch}

trait FEvalOption extends Option[FEvalCase, FEval] with FEvalArr {
  override def none[A](implicit ai: FEvalCase[A]) =
    new FEval[scala.Option[A]] {
      override val fec = optionInfo(ai)

      override def term[G: Gradient] = base.none(ai.wgi[G])
    }

  override def some[A](implicit ai: FEvalCase[A]) =
    new FEval[A => scala.Option[A]] {
      override val fec = aInfo(ai, optionInfo(ai))

      override def term[G: Gradient] = base.some(ai.wgi[G])
    }

  override def optionMatch[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[scala.Option[A] => B => (A => B) => B] {
      override val fec = aInfo(optionInfo(ai), aInfo(bi, aInfo(aInfo(ai, bi), bi)))

      override def term[G: Gradient] = base.optionMatch(ai.wgi[G], bi.wgi[G])
    }

  override implicit def optionInfo[A](implicit ai: FEvalCase[A]):
  FEvalCase.Aux[scala.Option[A], Lambda[G => scala.Option[ai.WithGrad[G]]]] =
    new FEvalCase[scala.Option[A]] {
      override type WithGrad[G] = scala.Option[ai.WithGrad[G]]

      override val tm = ofem[A]

      override def tmr: tm.ret = ai

      override def wgi[G: Gradient] = base.optionInfo(ai.wgi[G])
    }

  override def optionElmInfo[A]: FEvalCase[scala.Option[A]] => FEvalCase[A] = _.get(ofem[A])

  def ofem[A] = new FEvalMatch[scala.Option[A]] {
    override type ret = FEvalCase[A]
  }
}

object FEvalOption extends FEvalOption