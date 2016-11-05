package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.FEvalArr
import com.thoughtworks.DDF.Language.LangInfoG
import com.thoughtworks.DDF.{FEMMatch, FEval, FEvalCase}

trait FEvalOption[G] extends Option[FEvalCase[G, ?], FEval[G, ?]] with FEvalArr[G] {
  override def none[A](implicit ai: FEvalCase[G, A]) =
    new FEval[G, scala.Option[A]] {
      override val tm = optionInfo(ai)

      override val deriv = base.none(ai.lr)
    }

  override def some[A](implicit ai: FEvalCase[G, A]) =
    new FEval[G, A => scala.Option[A]] {
      override val tm = aInfo(ai, optionInfo(ai))

      override val deriv = base.some(ai.lr)
    }

  override def optionMatch[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, scala.Option[A] => B => (A => B) => B] {
      override val tm = aInfo(optionInfo(ai), aInfo(bi, aInfo(aInfo(ai, bi), bi)))

      override val deriv = base.optionMatch(ai.lr, bi.lr)
    }

  override implicit def optionInfo[A](implicit ai: FEvalCase[G, A]):
  FEvalCase.Aux[G, scala.Option[A], scala.Option[ai.ret]] =
    new FEvalCase[G, scala.Option[A]] {
      override type ret = scala.Option[ai.ret]

      override val tm = ofem[A]

      override def tmr: tm.ret = ai

      override def lr: LangInfoG[scala.Option[ai.ret]] = base.optionInfo(ai.lr)
    }

  override def optionElmInfo[A]: FEvalCase[G, scala.Option[A]] => FEvalCase[G, A] = _.get(ofem[A])

  def ofem[A] = new FEMMatch[G, scala.Option[A]] {
    override type ret = FEvalCase[G, A]
  }
}

object FEvalOption {
  implicit def apply[G] = new FEvalOption[G] { }
}