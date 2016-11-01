package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowBot extends Bot[NoInfo, Show] with SimpleBot[Show] with ShowArr {
  override def exfalso[A](implicit ai: NoInfo[A]) = Show("Exfalso!")
}

object ShowBot extends ShowBot
