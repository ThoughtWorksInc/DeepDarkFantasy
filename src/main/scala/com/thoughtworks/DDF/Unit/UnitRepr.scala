package com.thoughtworks.DDF.Unit

trait UnitRepr[Info[_], Repr[_]] extends UnitInfo[Info, Repr] {
  def mkUnit: Repr[Unit]
}
