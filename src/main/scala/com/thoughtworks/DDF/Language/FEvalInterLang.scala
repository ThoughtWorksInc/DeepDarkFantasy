package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bot.FEvalBotMin
import com.thoughtworks.DDF.Combinators.FEvalComb
import com.thoughtworks.DDF.{FEval, FEvalCase, Gradient}
import com.thoughtworks.DDF.IO.FEvalIO
import com.thoughtworks.DDF.List.FEvalList
import com.thoughtworks.DDF.Option.FEvalOption
import com.thoughtworks.DDF.Product.FEvalProd
import com.thoughtworks.DDF.Sum.FEvalSum
import com.thoughtworks.DDF.Top.FEvalTop

trait FEvalInterLang extends
  InterLang[FEvalCase, FEval] with
  FEvalProd with
  FEvalList with
  FEvalComb with
  FEvalSum with
  FEvalOption with
  FEvalBotMin with
  FEvalIO with
  FEvalTop {
  override val base = LangTermLang
}

object FEvalInterLang extends FEvalInterLang