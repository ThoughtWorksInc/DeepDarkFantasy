package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bool.Bool
import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Cont.Cont
import com.thoughtworks.DDF.Double.Double
import com.thoughtworks.DDF.List.List
import com.thoughtworks.DDF.Option.Option
import com.thoughtworks.DDF.Product.Product
import com.thoughtworks.DDF.Sum.Sum
import com.thoughtworks.DDF.Unit.Unit

trait Lang[Info[_], Repr[_]] extends
  LangInfo[Info, Repr] with
  Product[Info, Repr] with
  Double[Info, Repr] with
  Option[Info, Repr] with
  Unit[Info, Repr] with
  List[Info, Repr] with
  Bool[Info, Repr] with
  Comb[Info, Repr] with
  Cont[Info, Repr] with
  Sum[Info, Repr]
