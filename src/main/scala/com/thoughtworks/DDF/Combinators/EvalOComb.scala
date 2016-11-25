package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.EvalOArr
import com.thoughtworks.DDF.EvalO
import com.thoughtworks.DDF.Language.InterLangInfoG

trait EvalOComb extends
  EvalOArr with
  Comb[InterLangInfoG, EvalO] {
  override def B[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]):
  EvalO[(B => C) => (A => B) => A => C] =
    aeval(ltl.B[A, B, C])(f => aeval(ltl.B_[A, B, C](f.l))(g => aeval(ltl.B__(f.l)(g.l))(x => app(f)(app(g)(x)))))

  override def C[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]):
  EvalO[(A => B => C) => B => A => C] =
    aeval(ltl.C[A, B, C])(f => aeval(ltl.C_(f.l))(b => aeval(ltl.C__(f.l)(b.l))(a => app(app(f)(a))(b))))

  override def K[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[A => B => A] =
    aeval(ltl.K[A, B])(a => aeval(ltl.K_[A, B](a.l))(_ => a))

  override def W[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[(A => A => B) => A => B] =
    aeval(ltl.W[A, B])(f => aeval(ltl.W_[A, B](f.l))(a => app(app(f)(a))(a)))

  override def App[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[(A => B) => A => B] = I[A => B]

  override def Let[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[A => (A => B) => B] =
    aeval(ltl.Let[A, B])(a => aeval(ltl.Let_[A, B](a.l))(f => app(f)(a)))

  override def S[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]):
  EvalO[(A => B => C) => (A => B) => A => C] =
    aeval(ltl.S[A, B, C])(f => aeval(ltl.S_(f.l))(x => aeval(ltl.S__(f.l)(x.l))(a => app(app(f)(a))(app(x)(a)))))

  override def I[A](implicit ai: InterLangInfoG[A]): EvalO[A => A] = aeval(ltl.I[A])(identity[EvalO[A]])

  override def Y[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[((A => B) => A => B) => A => B] =
    aeval(ltl.Y[A, B])(f => aeval(ltl.Y_(f.l))(a => app(app(f)(Y_(f)))(a)))
}

object EvalOComb extends EvalOComb