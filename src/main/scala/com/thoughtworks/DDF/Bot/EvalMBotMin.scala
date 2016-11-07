package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Arrow.EvalMArr
import com.thoughtworks.DDF.NoInfo

trait EvalMBotMin extends
  BotMin[NoInfo, Lambda[X => X]] with
  SimpleBot[Lambda[X => X]] with
  EvalMArr {
  override def exfalso[A](implicit ai: NoInfo[A]): Nothing => A = x => x
}

object EvalMBotMin extends EvalMBotMin