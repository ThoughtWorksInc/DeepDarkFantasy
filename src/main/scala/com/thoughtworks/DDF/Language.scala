package com.thoughtworks.DDF

import com.thoughtworks.DDF.Combinators._

trait Language[Info[_], Repr[_]] extends
  PairLang[Info, Repr] with
  SumLang[Info, Repr] with
  ArrLang[Info, Repr] with
  DoubleLang[Info, Repr] with
  Combinators[Info, Repr]
