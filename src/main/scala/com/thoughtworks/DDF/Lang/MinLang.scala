package com.thoughtworks.DDF.Lang

import com.thoughtworks.DDF.Arrow.ArrowRepr
import com.thoughtworks.DDF.Bool.BoolRepr
import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Double.DoubleRepr
import com.thoughtworks.DDF.List.ListBasic
import com.thoughtworks.DDF.Option.OptionRepr
import com.thoughtworks.DDF.Product.ProductBasic
import com.thoughtworks.DDF.Sum.SumRepr
import com.thoughtworks.DDF.Unit.UnitRepr

trait MinLang[Info[_], Repr[_]] extends
  LangInfo[Info, Repr] with
  ProductBasic[Info, Repr] with
  DoubleRepr[Info, Repr] with
  OptionRepr[Info, Repr] with
  ArrowRepr[Info, Repr] with
  UnitRepr[Info, Repr] with
  ListBasic[Info, Repr] with
  BoolRepr[Info, Repr] with
  SumRepr[Info, Repr] with
  Comb[Info, Repr]
