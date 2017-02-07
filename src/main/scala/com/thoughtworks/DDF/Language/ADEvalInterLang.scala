package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bot.ADEvalBotMin
import com.thoughtworks.DDF.Combinators.ADEvalComb
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.{ADEval, ADEvalCase, ADEvalMatch}
import com.thoughtworks.DDF.IO.ADEvalIO
import com.thoughtworks.DDF.List.ADEvalList
import com.thoughtworks.DDF.Option.ADEvalOption
import com.thoughtworks.DDF.Product.ADEvalProd
import com.thoughtworks.DDF.Stream.ADEvalStream
import com.thoughtworks.DDF.Sum.ADEvalSum
import com.thoughtworks.DDF.Top.ADEvalTop
import com.thoughtworks.DDF.RecursiveInfoMatch._
import com.thoughtworks.DDF.String.ADEvalString

trait ADEvalInterLang extends
  InterLang[ADEvalCase, ADEval] with
  ADEvalProd with
  ADEvalList with
  ADEvalComb with
  ADEvalSum with
  ADEvalOption with
  ADEvalBotMin with
  ADEvalIO with
  ADEvalTop with
  ADEvalStream with
  ADEvalString {
  override val base = LangTermLang
}

object ADEvalInterLang extends ADEvalInterLang