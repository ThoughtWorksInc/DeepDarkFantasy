package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.FEvalArr
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.{FEval, FEvalCase, FEvalMatch}
import com.thoughtworks.DDF.Language.LangTermLang
import com.thoughtworks.DDF.Top.FEvalTop

trait FEvalStream extends
  Stream[FEvalCase, FEval] with
  FEvalTop with
  FEvalArr {
  override val base = LangTermLang

  override def streamNil[A](implicit ai: FEvalCase[A]) =
    new FEval[scala.Stream[A]] {
      override val fec = streamInfo(ai)

      override def term[G: Gradient] = base.streamNil(ai.wgi[G])
    }

  override def streamCons[A](implicit ai: FEvalCase[A]) =
    new FEval[A => (Unit => scala.Stream[A]) => scala.Stream[A]] {
      override val fec = aInfo(ai, aInfo(aInfo(topInfo, streamInfo(ai)), streamInfo(ai)))

      override def term[G: Gradient] = base.streamCons(ai.wgi[G])
    }

  override def streamMatch[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[scala.Stream[A] => B => (A => scala.Stream[A] => B) => B] {
      override val fec = aInfo(streamInfo(ai), aInfo(bi, aInfo(aInfo(ai, aInfo(streamInfo(ai), bi)), bi)))

      override def term[G: Gradient] = base.streamMatch(ai.wgi[G], bi.wgi[G])
    }

  private def sfem[A] = new FEvalMatch[scala.Stream[A]] {
    override type ret = FEvalCase[A]
  }

  override implicit def streamInfo[A](implicit ai: FEvalCase[A]):
  FEvalCase.Aux[scala.Stream[A], Lambda[G => scala.Stream[ai.WithGrad[G]]]] =
    new FEvalCase[scala.Stream[A]] {
      override type WithGrad[G] = scala.Stream[ai.WithGrad[G]]

      override val tm = sfem[A]

      override def tmr = ai

      override def wgi[G: Gradient] = base.streamInfo(ai.wgi[G])
    }

  override def streamElmInfo[A]: FEvalCase[scala.Stream[A]] => FEvalCase[A] = _.get(sfem[A])
}

object FEvalStream extends FEvalStream