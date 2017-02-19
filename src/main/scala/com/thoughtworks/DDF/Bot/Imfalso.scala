package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Top.Top

trait Imfalso extends BotMin with Top {
  def imfalso[A <: Type: Kind]: Top ~>: A

  final def imfalso_[A <: Type: Kind](t: Top) = app(imfalso[A])(t)
}
