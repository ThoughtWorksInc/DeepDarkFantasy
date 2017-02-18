package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.LangBase.SimpleLangBase
import com.thoughtworks.DDF.NoInfo

trait SimpleBot[Repr[_]] extends BotType[NoInfo, Repr] with SimpleLangBase[Repr] {
  override implicit def botInfo = NoInfo()
}

object SimpleBot {
  implicit def apply[Repr[_]] = new SimpleBot[Repr] {}
}