package com.thoughtworks.DDF

import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm, LangTermLang}

package object Gradient {
  trait Gradient[G] {
    trait GCD {
      def gc: LangTerm[Double => G]

      def gd: LangTerm[G => Double]

      def law(x: SemEq[Double => Double]): Prop = x.semEq(ltl.B__(gd)(gc))(ltl.I(di))
    }

    val GCDS: Stream[GCD]

    implicit val ltl = LangTermLang

    implicit val ti = ltl.topInfo

    implicit val di = ltl.doubleInfo

    implicit def pi[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): LangInfoG[(A, B)] = ltl.prodInfo(ai, bi)

    implicit def ai[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): LangInfoG[A => B] = ltl.aInfo(ai, bi)

    implicit def GInfo: LangInfoG[G]

    def constG: LangTerm[G]

    def mult: LangTerm[Double => G => G]

    final def mult_ : LangTerm[Double] => LangTerm[G => G] = ltl.app(mult)

    final def mult__ : LangTerm[Double] => LangTerm[G] => LangTerm[G] = d => ltl.app(mult_(d))

    def plus: LangTerm[G => G => G]

    final def plus_ : LangTerm[G] => LangTerm[G => G] = ltl.app(plus)

    final def plus__ : LangTerm[G] => LangTerm[G] => LangTerm[G] = l => ltl.app(plus_(l))
  }

}
