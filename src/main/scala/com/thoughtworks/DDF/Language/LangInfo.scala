package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Arrow.ArrInfo
import com.thoughtworks.DDF.Bool.BoolInfo
import com.thoughtworks.DDF.Double.DoubleInfo
import com.thoughtworks.DDF.List.ListInfo
import com.thoughtworks.DDF.Option.OptionInfo
import com.thoughtworks.DDF.Product.ProductInfo
import com.thoughtworks.DDF.Sum.SumInfo
import com.thoughtworks.DDF.Unit.UnitInfo

trait LangInfo[Info[_], Repr[_]] extends
  ProductInfo[Info, Repr] with
  DoubleInfo[Info, Repr] with
  OptionInfo[Info, Repr] with
  ArrInfo[Info, Repr] with
  UnitInfo[Info, Repr] with
  ListInfo[Info, Repr] with
  BoolInfo[Info, Repr] with
  SumInfo[Info, Repr]
