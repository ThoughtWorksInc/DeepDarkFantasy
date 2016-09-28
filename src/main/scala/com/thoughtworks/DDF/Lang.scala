package com.thoughtworks.DDF

import com.thoughtworks.DDF.Arr.ArrLang
import com.thoughtworks.DDF.Combinators._
import com.thoughtworks.DDF.Double.DLang
import com.thoughtworks.DDF.List.ListLang
import com.thoughtworks.DDF.Product.ProdLang
import com.thoughtworks.DDF.Sum.SumLang
import com.thoughtworks.DDF.Unit.UnitLang

trait Lang[Info[_], Repr[_]] extends
  ProdLang[Info, Repr] with
  UnitLang[Info, Repr] with
  ListLang[Info, Repr] with
  SumLang[Info, Repr] with
  ArrLang[Info, Repr] with
  DLang[Info, Repr] with
  Comb[Info, Repr]
