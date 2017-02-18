package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Arrow.Arr

trait BotMin extends Arr with BotType {
  def exfalso[A <: Type: Kind]: Bot ~>: A

  final def exfalso_[A <: Type: Kind](b: Bot) = app[Bot, A](exfalso[A])(b)
}
