package com.thoughtworks.DDF

import com.thoughtworks.DDF.Arr.ArrLang
import com.thoughtworks.DDF.Combinators._
import com.thoughtworks.DDF.Double.DoubleLang
import com.thoughtworks.DDF.Pair.PairLang
import com.thoughtworks.DDF.Sum.SumLang

trait Language[Info[_], Repr[_]] extends
  PairLang[Info, Repr] with
  SumLang[Info, Repr] with
  ArrLang[Info, Repr] with
  DoubleLang[Info, Repr] with
  Combinators[Info, Repr]
