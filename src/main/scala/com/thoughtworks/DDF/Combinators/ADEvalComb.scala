package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.ADEvalArr
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.Language.LangTermLang
import com.thoughtworks.DDF.{ADEval, ADEvalCase}

trait ADEvalComb extends Comb[ADEvalCase, ADEval] with ADEvalArr {
  override val base = LangTermLang

  override def Let[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[A => (A => B) => B] {
      override val fec = aInfo(ai, aInfo(aInfo(ai, bi), bi))

      override def term[G: Gradient] = base.Let(ai.wgi[G], bi.wgi[G])
    }

  override def K[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) = new ADEval[A => B => A] {
    override val fec = aInfo(ai, aInfo(bi, ai))

    override def term[G: Gradient] = base.K(ai.wgi[G], bi.wgi[G])
  }

  override def W[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[(A => A => B) => A => B] {
      override val fec = aInfo(aInfo(ai, aInfo(ai, bi)), aInfo(ai, bi))

      override def term[G: Gradient] = base.W(ai.wgi[G], bi.wgi[G])
    }

  override def C[A, B, C](implicit ai: ADEvalCase[A], bi: ADEvalCase[B], ci: ADEvalCase[C]) =
    new ADEval[(A => B => C) => B => A => C] {
      override val fec = aInfo(aInfo(ai, aInfo(bi, ci)), aInfo(bi, aInfo(ai, ci)))

      override def term[G: Gradient] = base.C(ai.wgi[G], bi.wgi[G], ci.wgi[G])
    }

  override def B[A, B, C](implicit ai: ADEvalCase[A], bi: ADEvalCase[B], ci: ADEvalCase[C]) =
    new ADEval[(B => C) => (A => B) => A => C] {
      override val fec = aInfo(aInfo(bi, ci), aInfo(aInfo(ai, bi), aInfo(ai, ci)))

      override def term[G: Gradient] = base.B(ai.wgi[G], bi.wgi[G], ci.wgi[G])
    }

  override def S[A, B, C](implicit ai: ADEvalCase[A], bi: ADEvalCase[B], ci: ADEvalCase[C]) =
    new ADEval[(A => B => C) => (A => B) => A => C] {
      override val fec = aInfo(aInfo(ai, aInfo(bi, ci)), aInfo(aInfo(ai, bi), aInfo(ai, ci)))

      override def term[G: Gradient] = base.S(ai.wgi[G], bi.wgi[G], ci.wgi[G])
    }

  override def App[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]): ADEval[(A => B) => A => B] =
    new ADEval[(A => B) => A => B] {
      override val fec = aInfo(aInfo(ai, bi), aInfo(ai, bi))

      override def term[G: Gradient] = base.App(ai.wgi[G], bi.wgi[G])
    }

  override def I[A](implicit ai: ADEvalCase[A]): ADEval[A => A] = new ADEval[A => A] {
    override val fec = aInfo(ai, ai)

    override def term[G: Gradient] = base.I(ai.wgi[G])
  }

  override def Y[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[((A => B) => A => B) => A => B] {
      override val fec = aInfo(aInfo(aInfo(ai, bi), aInfo(ai, bi)), aInfo(ai, bi))

      override def term[G: Gradient] = base.Y(ai.wgi[G], bi.wgi[G])
    }
}

object ADEvalComb extends ADEvalComb