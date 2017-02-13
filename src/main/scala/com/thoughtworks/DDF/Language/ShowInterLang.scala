package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.Bool.ShowBool
import com.thoughtworks.DDF.Bot.ShowBotMin
import com.thoughtworks.DDF.Combinators.ShowComb
import com.thoughtworks.DDF.Double.ShowDoubleMin
import com.thoughtworks.DDF.IO.ShowIO
import com.thoughtworks.DDF.List.ShowList
import com.thoughtworks.DDF.{NoInfo, Show}
import com.thoughtworks.DDF.Option.ShowOption
import com.thoughtworks.DDF.Product.ShowProd
import com.thoughtworks.DDF.Stream.ShowStream
import com.thoughtworks.DDF.Sum.ShowSum
import com.thoughtworks.DDF.Top.ShowTop
import com.thoughtworks.DDF.String.ShowString

trait ShowInterLang extends
  InterLang[NoInfo, Lambda[X => Show]] with
  SimpleLang[Lambda[X => Show]] with
  ShowComb with
  ShowDoubleMin with
  ShowProd with
  ShowList with
  ShowSum with
  ShowTop with
  ShowBool with
  ShowOption with
  ShowBotMin with
  ShowArr with
  ShowStream with
  ShowIO with
  ShowString

object ShowInterLang extends ShowInterLang