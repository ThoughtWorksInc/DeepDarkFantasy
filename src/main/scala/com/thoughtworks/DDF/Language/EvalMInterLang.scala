package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bot.EvalMBotMin
import com.thoughtworks.DDF.Combinators.EvalMComb
import com.thoughtworks.DDF.Double.EvalMDoubleMin
import com.thoughtworks.DDF.IO.EvalMIO
import com.thoughtworks.DDF.List.EvalMList
import com.thoughtworks.DDF.NoInfo
import com.thoughtworks.DDF.Option.EvalMOption
import com.thoughtworks.DDF.Stream.EvalMStream
import com.thoughtworks.DDF.Sum.EvalMSum

trait EvalMInterLang extends
  InterLang[NoInfo, Lambda[X => X]] with
  SimpleLang[Lambda[X => X]] with
  EvalMComb with
  EvalMList with
  EvalMDoubleMin with
  EvalMOption with
  EvalMIO with
  EvalMSum with
  EvalMStream with
  EvalMBotMin

object EvalMInterLang extends EvalMInterLang