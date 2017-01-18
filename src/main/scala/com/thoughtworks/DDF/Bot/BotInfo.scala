package com.thoughtworks.DDF.Bot

trait BotInfo[Info[_], Repr[_]] {
  implicit def botInfo: Info[Nothing]
}
