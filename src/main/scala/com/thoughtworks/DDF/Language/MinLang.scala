package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Arrow.Arrow
import com.thoughtworks.DDF.Bool.Bool
import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Double.Double
import com.thoughtworks.DDF.List.ListMin
import com.thoughtworks.DDF.Option.Option
import com.thoughtworks.DDF.Product.ProductMin
import com.thoughtworks.DDF.Sum.Sum
import com.thoughtworks.DDF.Unit.Unit

trait MinLang[Info[_], Repr[_]] extends
  LangInfo[Info, Repr] with
  ProductMin[Info, Repr] with
  Double[Info, Repr] with
  Option[Info, Repr] with
  Arrow[Info, Repr] with
  Unit[Info, Repr] with
  ListMin[Info, Repr] with
  Bool[Info, Repr] with
  Sum[Info, Repr] with
  Comb[Info, Repr]
