package com.thoughtworks.DDF.Lang

import com.thoughtworks.DDF.Bool.ShowBool
import com.thoughtworks.DDF.Combinators.ShowComb
import com.thoughtworks.DDF.Double.ShowDouble
import com.thoughtworks.DDF.Forall.ShowForall
import com.thoughtworks.DDF.List.ShowList
import com.thoughtworks.DDF.Option.ShowOption
import com.thoughtworks.DDF.Product.ShowProduct
import com.thoughtworks.DDF.Sum.ShowSum
import com.thoughtworks.DDF.Unit.ShowUnit
import com.thoughtworks.DDF.Show

trait ShowLang extends
  SimpleLang[Show] with
  ShowComb with
  ShowDouble with
  ShowProduct with
  ShowList with
  ShowSum with
  ShowUnit with
  ShowBool with
  ShowOption with
  ShowForall

object ShowLang {
  implicit def apply = new ShowLang {}
}


