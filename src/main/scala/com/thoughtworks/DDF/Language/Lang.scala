package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Arrow.ArrowRepr
import com.thoughtworks.DDF.Bool.BoolRepr
import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Double.Double
import com.thoughtworks.DDF.List.List
import com.thoughtworks.DDF.Option.OptionRepr
import com.thoughtworks.DDF.Product.ProductRepr
import com.thoughtworks.DDF.Sum.Sum
import com.thoughtworks.DDF.Unit.Unit

trait Lang[Info[_], Repr[_]] extends
  LangInfo[Info, Repr] with
  ProductRepr[Info, Repr] with
  Double[Info, Repr] with
  OptionRepr[Info, Repr] with
  ArrowRepr[Info, Repr] with
  Unit[Info, Repr] with
  List[Info, Repr] with
  BoolRepr[Info, Repr] with
  Sum[Info, Repr] with
  Comb[Info, Repr]
