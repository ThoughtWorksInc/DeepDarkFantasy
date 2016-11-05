package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bot.FEvalBotMin
import com.thoughtworks.DDF.Combinators.FEvalComb
import com.thoughtworks.DDF.IO.FEvalIO
import com.thoughtworks.DDF.List.FEvalList
import com.thoughtworks.DDF.Option.FEvalOption
import com.thoughtworks.DDF.Product.FEvalProd
import com.thoughtworks.DDF.Sum.FEvalSum
import com.thoughtworks.DDF.Top.FEvalTop
import com.thoughtworks.DDF.{FEval, FEvalCase}

trait FEvalInterLang[G] extends
  InterLang[FEvalCase[G, ?], FEval[G, ?]] with
  FEvalProd[G] with
  FEvalList[G] with
  FEvalComb[G] with
  FEvalSum[G] with
  FEvalOption[G] with
  FEvalBotMin[G] with
  FEvalIO[G] with
  FEvalTop[G] {
  override val base = LangTermLang
}

object FEvalInterLang {
  implicit def apply[G]: FEvalInterLang[G] = new FEvalInterLang[G] { }
}