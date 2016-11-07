package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.FEvalArr
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}
import com.thoughtworks.DDF.{FEval, FEvalCase, FEvalMatch, Gradient}

trait FEvalSum extends Sum[FEvalCase, FEval] with FEvalArr {
  override def sumAssocRL[A, B, C](implicit ai: FEvalCase[A], bi: FEvalCase[B], ci: FEvalCase[C]) =
    new FEval[Either[A, Either[B, C]] => Either[Either[A, B], C]] {
      override val fec = aInfo(sumInfo(ai, sumInfo(bi, ci)), sumInfo(sumInfo(ai, bi), ci))

      override def term[G: Gradient] = base.sumAssocRL(ai.wgi[G], bi.wgi[G], ci.wgi[G])
    }

  override def sumMatch[A, B, C](implicit ai: FEvalCase[A], bi: FEvalCase[B], ci: FEvalCase[C]) =
    new FEval[Either[A, B] => (A => C) => (B => C) => C] {
      override val fec = aInfo(sumInfo(ai, bi), aInfo(aInfo(ai, ci), aInfo(aInfo(bi, ci), ci)))

      override def term[G: Gradient] = base.sumMatch(ai.wgi[G], bi.wgi[G], ci.wgi[G])
    }

  override def left[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[A => Either[A, B]] {
      override val fec = aInfo(ai, sumInfo(ai, bi))

      override def term[G: Gradient] = base.left(ai.wgi[G], bi.wgi[G])
    }

  override def sumComm[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[Either[A, B] => Either[B, A]] {
      override val fec = aInfo(sumInfo(ai, bi), sumInfo(bi, ai))

      override def term[G: Gradient] = base.sumComm(ai.wgi[G], bi.wgi[G])
    }

  override implicit def sumInfo[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]):
  FEvalCase.Aux[Either[A, B], Lambda[G => Either[ai.WithGrad[G], bi.WithGrad[G]]]] =
    new FEvalCase[Either[A, B]] {
      override type WithGrad[G] = Either[ai.WithGrad[G], bi.WithGrad[G]]

      override def wgi[G: Gradient] = base.sumInfo(ai.wgi[G], bi.wgi[G])

    override val tm = sfem[A, B]

    override def tmr: tm.ret = (ai, bi)
  }

  override def sumLeftInfo[A, B]: FEvalCase[Either[A, B]] => FEvalCase[A] = _.get(sfem[A, B])._1

  override def sumRightInfo[A, B]: FEvalCase[Either[A, B]] => FEvalCase[B] = _.get(sfem[A, B])._2

  override def right[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[B => Either[A, B]] {
      override val fec = aInfo(bi, sumInfo(ai, bi))

      override def term[G: Gradient] = base.right(ai.wgi[G], bi.wgi[G])
    }

  def sfem[A, B] = new FEvalMatch[Either[A, B]] {
    override type ret = (FEvalCase[A], FEvalCase[B])
  }

  override def sumAssocLR[A, B, C](implicit ai: FEvalCase[A], bi: FEvalCase[B], ci: FEvalCase[C]) =
    new FEval[Either[Either[A, B], C] => Either[A, Either[B, C]]] {
      override val fec = aInfo(sumInfo(sumInfo(ai, bi), ci), sumInfo(ai, sumInfo(bi, ci)))

      override def term[G: Gradient] = base.sumAssocLR(ai.wgi[G], bi.wgi[G], ci.wgi[G])
    }
}

object FEvalSum extends FEvalSum