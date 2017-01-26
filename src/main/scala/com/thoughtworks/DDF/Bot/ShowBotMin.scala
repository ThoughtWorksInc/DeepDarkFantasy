package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.{NoInfo, ShowLeaf}

trait ShowBotMin extends BotMin[NoInfo, ShowLeaf] with SimpleBot[ShowLeaf] with ShowArr {
  override def exfalso[A](implicit ai: NoInfo[A]) = ShowLeaf("Exfalso!")
}

object ShowBotMin extends ShowBotMin
