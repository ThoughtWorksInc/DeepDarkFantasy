package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bool.BEvalBool
import com.thoughtworks.DDF.Bot.BEvalBot
import com.thoughtworks.DDF.Combinators.BEvalComb
import com.thoughtworks.DDF.Double.BEvalDouble
import com.thoughtworks.DDF.List.BEvalList
import com.thoughtworks.DDF.Option.BEvalOption
import com.thoughtworks.DDF.Product.BEvalProd
import com.thoughtworks.DDF.Sum.BEvalSum
import com.thoughtworks.DDF.Top.BEvalTop
import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalInterLang extends
  InterLang[LossInfo, BEval] with
  BEvalProd with
  BEvalComb with
  BEvalDouble with
  BEvalSum with
  BEvalList with
  BEvalTop with
  BEvalBool with
  BEvalOption with
  BEvalBot

object BEvalInterLang extends BEvalInterLang