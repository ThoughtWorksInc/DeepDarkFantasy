package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.ADEvalArr
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.{ADEval, ADEvalCase, ADEvalMatch}
import com.thoughtworks.DDF.Language.LangTermLang
import com.thoughtworks.DDF.Top.ADEvalTop
import com.thoughtworks.DDF.RecursiveInfoMatch._

trait ADEvalStream extends
  Stream[ADEvalCase, ADEval] with
  ADEvalTop with
  ADEvalArr {
  override val base = LangTermLang

  override def streamNil[A](implicit ai: ADEvalCase[A]) =
    new ADEval[scala.Stream[A]] {
      override val fec = streamInfo(ai)

      override def term[G: Gradient] = base.streamNil(ai.wgi[G])
    }

  override def streamCons[A](implicit ai: ADEvalCase[A]) =
    new ADEval[A => (Unit => scala.Stream[A]) => scala.Stream[A]] {
      override val fec = aInfo(ai, aInfo(aInfo(topInfo, streamInfo(ai)), streamInfo(ai)))

      override def term[G: Gradient] = base.streamCons(ai.wgi[G])
    }

  override def streamMatch[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[scala.Stream[A] => B => (A => scala.Stream[A] => B) => B] {
      override val fec = aInfo(streamInfo(ai), aInfo(bi, aInfo(aInfo(ai, aInfo(streamInfo(ai), bi)), bi)))

      override def term[G: Gradient] = base.streamMatch(ai.wgi[G], bi.wgi[G])
    }

  override implicit def streamInfo[A](implicit ai: ADEvalCase[A]):
  ADEvalCase.Aux[scala.Stream[A], Lambda[G => scala.Stream[ai.WithGrad[G]]]] =
    new ADEvalCase[scala.Stream[A]] with StreamRI[ADEvalMatch, ADEvalCase, A] {
      override type WithGrad[G] = scala.Stream[ai.WithGrad[G]]

      override def tmr = ai

      override def wgi[G: Gradient] = base.streamInfo(ai.wgi[G])
    }

  override def streamElmInfo[A]: ADEvalCase[scala.Stream[A]] => ADEvalCase[A] = _.get(StM[ADEvalMatch, ADEvalCase, A])
}

object ADEvalStream extends ADEvalStream