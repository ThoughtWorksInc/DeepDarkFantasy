package com.thoughtworks.DDF.Unit

trait Unit[Info[_], Repr[_]] extends UnitInfo[Info, Repr] {
  def mkUnit: Repr[Unit]
}
