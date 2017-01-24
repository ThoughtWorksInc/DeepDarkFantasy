package com.thoughtworks.DDF

import scalaz.Leibniz._

trait TypeCase[Match <: TypeMatch[Match]] {
  val tm: Match

  def tmr: tm.ret

  def get[Z](implicit tmz: Match {type ret = Z}): Z = witness(tm.unique(tmz))(tmr)
}
