package com.thoughtworks.DDF

import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm, LangTermLang}

package object Gradient {
  trait Gradient[G] {
    implicit val ltl = LangTermLang

    implicit def GInfo: LangInfoG[G]

    def constG: LangTerm[G]

    def plus: LangTerm[((Double, G)) => ((Double, G)) => (Double, G)]

    def mult: LangTerm[((Double, G)) => ((Double, G)) => (Double, G)]

    def div: LangTerm[((Double, G)) => ((Double, G)) => (Double, G)]

    def sig: LangTerm[((Double, G)) => (Double, G)]

    def exp: LangTerm[((Double, G)) => (Double, G)]
  }

}
