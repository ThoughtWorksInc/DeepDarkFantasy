package com.thoughtworks.DDF.Lang

import com.thoughtworks.DDF.Combinators.ShowComb
import com.thoughtworks.DDF.Double.ShowD
import com.thoughtworks.DDF.List.ShowList
import com.thoughtworks.DDF.Product.ShowProd
import com.thoughtworks.DDF.Sum.ShowSum
import com.thoughtworks.DDF.Unit.ShowUnit
import com.thoughtworks.DDF.Show

trait ShowLang extends SimpleLang[Show] with ShowComb with ShowD with ShowProd with ShowList with ShowSum with ShowUnit

object ShowLang {
  implicit def apply = new ShowLang {}
}
