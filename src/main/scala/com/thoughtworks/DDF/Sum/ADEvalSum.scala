package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.ADEvalArr
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.{ADEval, ADEvalCase, ADEvalMatch}

trait ADEvalSum extends Sum[ADEvalCase, ADEval] with ADEvalArr {
  override def sumAssocRL[A, B, C](implicit ai: ADEvalCase[A], bi: ADEvalCase[B], ci: ADEvalCase[C]) =
    new ADEval[Either[A, Either[B, C]] => Either[Either[A, B], C]] {
      override val fec = aInfo(sumInfo(ai, sumInfo(bi, ci)), sumInfo(sumInfo(ai, bi), ci))

      override def term[G: Gradient] = base.sumAssocRL(ai.wgi[G], bi.wgi[G], ci.wgi[G])
    }

  override def sumMatch[A, B, C](implicit ai: ADEvalCase[A], bi: ADEvalCase[B], ci: ADEvalCase[C]) =
    new ADEval[Either[A, B] => (A => C) => (B => C) => C] {
      override val fec = aInfo(sumInfo(ai, bi), aInfo(aInfo(ai, ci), aInfo(aInfo(bi, ci), ci)))

      override def term[G: Gradient] = base.sumMatch(ai.wgi[G], bi.wgi[G], ci.wgi[G])
    }

  override def left[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[A => Either[A, B]] {
      override val fec = aInfo(ai, sumInfo(ai, bi))

      override def term[G: Gradient] = base.left(ai.wgi[G], bi.wgi[G])
    }

  override def sumComm[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[Either[A, B] => Either[B, A]] {
      override val fec = aInfo(sumInfo(ai, bi), sumInfo(bi, ai))

      override def term[G: Gradient] = base.sumComm(ai.wgi[G], bi.wgi[G])
    }

  override implicit def sumInfo[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]):
  ADEvalCase.Aux[Either[A, B], Lambda[G => Either[ai.WithGrad[G], bi.WithGrad[G]]]] =
    new ADEvalCase[Either[A, B]] {
      override type WithGrad[G] = Either[ai.WithGrad[G], bi.WithGrad[G]]

      override def wgi[G: Gradient] = base.sumInfo(ai.wgi[G], bi.wgi[G])

    override val tm = sfem[A, B]

    override def tmr: tm.ret = (ai, bi)
  }

  override def sumLeftInfo[A, B]: ADEvalCase[Either[A, B]] => ADEvalCase[A] = _.get(sfem[A, B])._1

  override def sumRightInfo[A, B]: ADEvalCase[Either[A, B]] => ADEvalCase[B] = _.get(sfem[A, B])._2

  override def right[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[B => Either[A, B]] {
      override val fec = aInfo(bi, sumInfo(ai, bi))

      override def term[G: Gradient] = base.right(ai.wgi[G], bi.wgi[G])
    }

  def sfem[A, B] = new ADEvalMatch[Either[A, B]] {
    override type ret = (ADEvalCase[A], ADEvalCase[B])
  }

  override def sumAssocLR[A, B, C](implicit ai: ADEvalCase[A], bi: ADEvalCase[B], ci: ADEvalCase[C]) =
    new ADEval[Either[Either[A, B], C] => Either[A, Either[B, C]]] {
      override val fec = aInfo(sumInfo(sumInfo(ai, bi), ci), sumInfo(ai, sumInfo(bi, ci)))

      override def term[G: Gradient] = base.sumAssocLR(ai.wgi[G], bi.wgi[G], ci.wgi[G])
    }
}

object ADEvalSum extends ADEvalSum