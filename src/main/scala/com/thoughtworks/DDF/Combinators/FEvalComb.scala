package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.FEvalArr
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.Language.LangTermLang
import com.thoughtworks.DDF.{FEval, FEvalCase}

trait FEvalComb extends Comb[FEvalCase, FEval] with FEvalArr {
  override val base = LangTermLang

  override def Let[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[A => (A => B) => B] {
      override val fec = aInfo(ai, aInfo(aInfo(ai, bi), bi))

      override def term[G: Gradient] = base.Let(ai.wgi[G], bi.wgi[G])
    }

  override def K[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) = new FEval[A => B => A] {
    override val fec = aInfo(ai, aInfo(bi, ai))

    override def term[G: Gradient] = base.K(ai.wgi[G], bi.wgi[G])
  }

  override def W[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[(A => A => B) => A => B] {
      override val fec = aInfo(aInfo(ai, aInfo(ai, bi)), aInfo(ai, bi))

      override def term[G: Gradient] = base.W(ai.wgi[G], bi.wgi[G])
    }

  override def C[A, B, C](implicit ai: FEvalCase[A], bi: FEvalCase[B], ci: FEvalCase[C]) =
    new FEval[(A => B => C) => B => A => C] {
      override val fec = aInfo(aInfo(ai, aInfo(bi, ci)), aInfo(bi, aInfo(ai, ci)))

      override def term[G: Gradient] = base.C(ai.wgi[G], bi.wgi[G], ci.wgi[G])
    }

  override def B[A, B, C](implicit ai: FEvalCase[A], bi: FEvalCase[B], ci: FEvalCase[C]) =
    new FEval[(B => C) => (A => B) => A => C] {
      override val fec = aInfo(aInfo(bi, ci), aInfo(aInfo(ai, bi), aInfo(ai, ci)))

      override def term[G: Gradient] = base.B(ai.wgi[G], bi.wgi[G], ci.wgi[G])
    }

  override def S[A, B, C](implicit ai: FEvalCase[A], bi: FEvalCase[B], ci: FEvalCase[C]) =
    new FEval[(A => B => C) => (A => B) => A => C] {
      override val fec = aInfo(aInfo(ai, aInfo(bi, ci)), aInfo(aInfo(ai, bi), aInfo(ai, ci)))

      override def term[G: Gradient] = base.S(ai.wgi[G], bi.wgi[G], ci.wgi[G])
    }

  override def App[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]): FEval[(A => B) => A => B] =
    new FEval[(A => B) => A => B] {
      override val fec = aInfo(aInfo(ai, bi), aInfo(ai, bi))

      override def term[G: Gradient] = base.App(ai.wgi[G], bi.wgi[G])
    }

  override def I[A](implicit ai: FEvalCase[A]): FEval[A => A] = new FEval[A => A] {
    override val fec = aInfo(ai, ai)

    override def term[G: Gradient] = base.I(ai.wgi[G])
  }

  override def Y[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[((A => B) => A => B) => A => B] {
      override val fec = aInfo(aInfo(aInfo(ai, bi), aInfo(ai, bi)), aInfo(ai, bi))

      override def term[G: Gradient] = base.Y(ai.wgi[G], bi.wgi[G])
    }
}

object FEvalComb extends FEvalComb