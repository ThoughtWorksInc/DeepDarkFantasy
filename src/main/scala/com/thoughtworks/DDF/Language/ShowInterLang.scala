package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.Bool.ShowBool
import com.thoughtworks.DDF.Bot.ShowBotMin
import com.thoughtworks.DDF.Combinators.ShowComb
import com.thoughtworks.DDF.Double.ShowDoubleMin
import com.thoughtworks.DDF.IO.ShowIO
import com.thoughtworks.DDF.List.ShowList
import com.thoughtworks.DDF.Option.ShowOption
import com.thoughtworks.DDF.Product.ShowProd
import com.thoughtworks.DDF.Stream.ShowStream
import com.thoughtworks.DDF.Sum.ShowSum
import com.thoughtworks.DDF.Top.ShowTop
import com.thoughtworks.DDF.{NoInfo, ShowLeaf}

trait ShowInterLang extends
  InterLang[NoInfo, ShowLeaf] with
  SimpleLang[ShowLeaf] with
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
  ShowIO {
  override def litString: (String) => ShowLeaf[String] = str => ShowLeaf("str: " + str)
}

object ShowInterLang extends ShowInterLang