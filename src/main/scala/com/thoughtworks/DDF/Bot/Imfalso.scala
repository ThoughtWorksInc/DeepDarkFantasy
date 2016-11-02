package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Top.Top

trait Imfalso[Info[_], Repr[_]] extends BotMin[Info, Repr] with Top[Info, Repr] {
  def imfalso[A](implicit ai: Info[A]): Repr[Unit => A]

  final def imfalso_[A](u: Repr[Unit])(implicit ai: Info[A]): Repr[A] = app(imfalso[A])(u)
}
