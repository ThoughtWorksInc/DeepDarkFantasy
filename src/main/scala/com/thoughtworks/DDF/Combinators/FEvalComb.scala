package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.FEvalArr
import com.thoughtworks.DDF.Language.{LangTerm, LangTermLang}
import com.thoughtworks.DDF.{FEval, FEvalCase}

trait FEvalComb[G] extends Comb[FEvalCase[G, ?], FEval[G, ?]] with FEvalArr[G] {
  override val base = LangTermLang

  override def Let[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, A => (A => B) => B] {
      override val tm = aInfo(ai, aInfo(aInfo(ai, bi), bi))

      override val deriv = base.Let(ai.lr, bi.lr)
    }

  override def K[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) = new FEval[G, A => B => A] {
    override val tm = aInfo(ai, aInfo(bi, ai))

    override val deriv = base.K(ai.lr, bi.lr)
  }

  override def W[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, (A => A => B) => A => B] {
      override val tm = aInfo(aInfo(ai, aInfo(ai, bi)), aInfo(ai, bi))

      override val deriv = base.W(ai.lr, bi.lr)
    }

  override def C[A, B, C](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B], ci: FEvalCase[G, C]) =
    new FEval[G, (A => B => C) => B => A => C] {
      override val tm = aInfo(aInfo(ai, aInfo(bi, ci)), aInfo(bi, aInfo(ai, ci)))

      override val deriv = base.C(ai.lr, bi.lr, ci.lr)
    }

  override def B[A, B, C](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B], ci: FEvalCase[G, C]) =
    new FEval[G, (B => C) => (A => B) => A => C] {
      override val tm = aInfo(aInfo(bi, ci), aInfo(aInfo(ai, bi), aInfo(ai, ci)))

      override val deriv = base.B(ai.lr, bi.lr, ci.lr)
    }

  override def S[A, B, C](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B], ci: FEvalCase[G, C]) =
    new FEval[G, (A => B => C) => (A => B) => A => C] {
      override val tm = aInfo(aInfo(ai, aInfo(bi, ci)), aInfo(aInfo(ai, bi), aInfo(ai, ci)))

      override val deriv = base.S(ai.lr, bi.lr, ci.lr)
    }

  override def App[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]): FEval[G, (A => B) => A => B] =
    new FEval[G, (A => B) => A => B] {
      override val tm = aInfo(aInfo(ai, bi), aInfo(ai, bi))

      override val deriv = base.App[ai.ret, bi.ret](ai.lr, bi.lr)
    }

  override def I[A](implicit ai: FEvalCase[G, A]): FEval[G, A => A] = new FEval[G, A => A] {
    override val tm = aInfo(ai, ai)

    override val deriv = base.I(ai.lr)
  }

  override def Y[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, ((A => B) => A => B) => A => B] {
      override val tm = aInfo(aInfo(aInfo(ai, bi), aInfo(ai, bi)), aInfo(ai, bi))

      override val deriv = base.Y(ai.lr, bi.lr)
    }
}

object FEvalComb {
  implicit def apply[G]: FEvalComb[G] = new FEvalComb[G] { }
}