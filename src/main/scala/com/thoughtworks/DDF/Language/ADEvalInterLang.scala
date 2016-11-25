package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bot.ADEvalBotMin
import com.thoughtworks.DDF.Combinators.ADEvalComb
import com.thoughtworks.DDF.{ADEval, ADEvalCase}
import com.thoughtworks.DDF.IO.ADEvalIO
import com.thoughtworks.DDF.List.ADEvalList
import com.thoughtworks.DDF.Option.ADEvalOption
import com.thoughtworks.DDF.Product.ADEvalProd
import com.thoughtworks.DDF.Stream.ADEvalStream
import com.thoughtworks.DDF.Sum.ADEvalSum
import com.thoughtworks.DDF.Top.ADEvalTop

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
  ADEvalStream {
  override val base = LangTermLang
}

object ADEvalInterLang extends ADEvalInterLang