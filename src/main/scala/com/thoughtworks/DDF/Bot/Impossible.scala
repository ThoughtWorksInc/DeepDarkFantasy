package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Top.Top

trait Impossible extends BotMin with Top {
  def impossible: Top ~>: Bot

  final def impossible_(t: Top): Bot = app(impossible)(t)

  final def impossible__ : Bot = impossible_(mkTop)
}
