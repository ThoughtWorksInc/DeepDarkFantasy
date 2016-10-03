package com.thoughtworks.DDF.Lang

import com.thoughtworks.DDF.Arrow.ArrowRepr
import com.thoughtworks.DDF.Combinators._
import com.thoughtworks.DDF.Double.DoubleRepr
import com.thoughtworks.DDF.List.ListRepr
import com.thoughtworks.DDF.Product.ProdRepr
import com.thoughtworks.DDF.Sum.SumRepr
import com.thoughtworks.DDF.Unit.UnitLang

trait Lang[Info[_], Repr[_]] extends
  ProdRepr[Info, Repr] with
  UnitLang[Info, Repr] with
  ListRepr[Info, Repr] with
  SumRepr[Info, Repr] with
  ArrowRepr[Info, Repr] with
  DoubleRepr[Info, Repr] with
  Comb[Info, Repr]
