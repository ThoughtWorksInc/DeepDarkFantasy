package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.LangBase.LangBase

trait BotType extends LangBase {
  type Bot <: Type

  implicit def BotK: Kind[Bot]
}
