package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.{ImpW, NoInfo}

trait ImpWLang[Info[_], Repr[_]] extends Lang[NoInfo, ImpW[Repr, ?]] {

}

object ImpWLang {
  implicit def apply[Info[_], Repr[_]]: ImpWLang[Info, Repr] = ???
}