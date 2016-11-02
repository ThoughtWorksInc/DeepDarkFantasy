package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bool.ShowBool
import com.thoughtworks.DDF.Bot.ShowBotMin
import com.thoughtworks.DDF.Combinators.ShowComb
import com.thoughtworks.DDF.Double.ShowDouble
import com.thoughtworks.DDF.List.ShowList
import com.thoughtworks.DDF.Option.ShowOption
import com.thoughtworks.DDF.Product.ShowProd
import com.thoughtworks.DDF.Sum.ShowSum
import com.thoughtworks.DDF.Top.ShowTop
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowLang extends
  MinLang[NoInfo, Show] with
  SimpleLang[Show] with
  ShowComb with
  ShowDouble with
  ShowProd with
  ShowList with
  ShowSum with
  ShowTop with
  ShowBool with
  ShowOption with
  ShowBotMin

object ShowLang {
  implicit def apply = new ShowLang {}
}