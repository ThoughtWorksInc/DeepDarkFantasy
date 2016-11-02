package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Arrow.ArrMin

trait BotMin[Info[_], Repr[_]] extends ArrMin[Info, Repr] with BotInfo[Info, Repr] {
  def exfalso[A](implicit ai: Info[A]): Repr[Nothing => A]

  final def exfalso_[A](b: Repr[Nothing])(implicit ai: Info[A]): Repr[A] = app[Nothing, A](exfalso[A])(b)
}
