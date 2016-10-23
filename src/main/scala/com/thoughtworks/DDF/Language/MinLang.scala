package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Arrow.ArrowRepr
import com.thoughtworks.DDF.Bool.BoolRepr
import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Double.DoubleRepr
import com.thoughtworks.DDF.List.ListMin
import com.thoughtworks.DDF.Option.OptionRepr
import com.thoughtworks.DDF.Product.ProductMin
import com.thoughtworks.DDF.Sum.Sum
import com.thoughtworks.DDF.Unit.Unit

trait MinLang[Info[_], Repr[_]] extends
  LangInfo[Info, Repr] with
  ProductMin[Info, Repr] with
  DoubleRepr[Info, Repr] with
  OptionRepr[Info, Repr] with
  ArrowRepr[Info, Repr] with
  Unit[Info, Repr] with
  ListMin[Info, Repr] with
  BoolRepr[Info, Repr] with
  Sum[Info, Repr] with
  Comb[Info, Repr]
