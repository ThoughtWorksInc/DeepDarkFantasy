package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.FEvalArr
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}
import com.thoughtworks.DDF.{FEMMatch, FEval, FEvalCase}

trait FEvalSum[G] extends Sum[FEvalCase[G, ?], FEval[G, ?]] with FEvalArr[G] {
  override def sumAssocRL[A, B, C](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B], ci: FEvalCase[G, C]) =
    new FEval[G, Either[A, Either[B, C]] => Either[Either[A, B], C]] {
      override val tm = aInfo(sumInfo(ai, sumInfo(bi, ci)), sumInfo(sumInfo(ai, bi), ci))

      override val deriv = base.sumAssocRL(ai.lr, bi.lr, ci.lr)
    }

  override def sumMatch[A, B, C](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B], ci: FEvalCase[G, C]) =
    new FEval[G, Either[A, B] => (A => C) => (B => C) => C] {
      override val tm = aInfo(sumInfo(ai, bi), aInfo(aInfo(ai, ci), aInfo(aInfo(bi, ci), ci)))

      override val deriv: LangTerm[tm.ret] = base.sumMatch(ai.lr, bi.lr, ci.lr)
    }

  override def left[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, A => Either[A, B]] {
      override val tm = aInfo(ai, sumInfo(ai, bi))

      override val deriv = base.left(ai.lr, bi.lr)
    }

  override def sumComm[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, Either[A, B] => Either[B, A]] {
      override val tm = aInfo(sumInfo(ai, bi), sumInfo(bi, ai))

      override val deriv: LangTerm[tm.ret] = base.sumComm(ai.lr, bi.lr)
    }

  override implicit def sumInfo[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]):
  FEvalCase.Aux[G, Either[A, B], Either[ai.ret, bi.ret]] = new FEvalCase[G, Either[A, B]] {
    override type ret = Either[ai.ret, bi.ret]

    override def lr: LangInfoG[Either[ai.ret, bi.ret]] = base.sumInfo(ai.lr, bi.lr)

    override val tm = sfem[A, B]

    override def tmr: tm.ret = (ai, bi)
  }

  override def sumLeftInfo[A, B]: FEvalCase[G, Either[A, B]] => FEvalCase[G, A] = _.get(sfem[A, B])._1

  override def sumRightInfo[A, B]: FEvalCase[G, Either[A, B]] => FEvalCase[G, B] = _.get(sfem[A, B])._2

  override def right[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, B => Either[A, B]] {
      override val tm = aInfo(bi, sumInfo(ai, bi))

      override val deriv = base.right(ai.lr, bi.lr)
    }

  def sfem[A, B] = new FEMMatch[G, Either[A, B]] {
    override type ret = (FEvalCase[G, A], FEvalCase[G, B])
  }

  override def sumAssocLR[A, B, C](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B], ci: FEvalCase[G, C]) =
    new FEval[G, Either[Either[A, B], C] => Either[A, Either[B, C]]] {
      override val tm = aInfo(sumInfo(sumInfo(ai, bi), ci), sumInfo(ai, sumInfo(bi, ci)))

      override val deriv = base.sumAssocLR(ai.lr, bi.lr, ci.lr)
    }
}

object FEvalSum {
  implicit def apply[G]: FEvalSum[G] = new FEvalSum[G] { }
}