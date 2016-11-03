package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Top.Top

trait Impossible[Info[_], Repr[_]] extends BotMin[Info, Repr] with Top[Info, Repr] {
  def impossible: Repr[Unit => Nothing]

  final def impossible_(u: Repr[Unit]): Repr[Nothing] = app[Unit, Nothing](impossible)(u)

  final def impossible__ : Repr[Nothing] = impossible_(mkTop)
}
