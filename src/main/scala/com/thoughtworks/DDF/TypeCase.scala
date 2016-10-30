package com.thoughtworks.DDF

import scalaz.Leibniz._

trait TypeCase[Match[Y] <: TypeMatch[Match, Y], X] {
  val tm: Match[X]

  def tmr: tm.ret

  def get[Z](implicit tmz: Match[X] {type ret = Z}): Z = witness(tm.unique(tmz))(tmr)
}
