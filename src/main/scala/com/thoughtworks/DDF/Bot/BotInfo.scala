package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.InfoBase.InfoBase

trait BotInfo[Info[_], Repr[_]] extends InfoBase[Info, Repr] {
  implicit def botInfo: Info[Nothing]
}
