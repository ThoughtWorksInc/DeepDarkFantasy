package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.InfoB.InfoB

trait UnitRepr[Info[_], Repr[_]] extends InfoB[Info, Repr] {
  def mkUnit: Repr[Unit]

  implicit def unitInfo: Info[Unit]
}
