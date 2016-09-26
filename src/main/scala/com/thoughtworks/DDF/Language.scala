package com.thoughtworks.DDF

import scala.language.higherKinds

trait Language[Info[_], Repr[_]] extends
  PairLang[Info, Repr] with
  SumLang[Info, Repr] with
  ArrLang[Info, Repr] with
  DoubleLang[Info, Repr] with
  SKILang[Info, Repr] with
  YLang[Info, Repr]
