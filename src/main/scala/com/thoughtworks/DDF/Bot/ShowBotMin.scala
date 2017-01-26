package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowBotMin extends BotMin[NoInfo, Lambda[X => Show]] with SimpleBot[Lambda[X => Show]] with ShowArr {
  override def exfalso[A](implicit ai: NoInfo[A]) = Show("Exfalso!")
}

object ShowBotMin extends ShowBotMin
