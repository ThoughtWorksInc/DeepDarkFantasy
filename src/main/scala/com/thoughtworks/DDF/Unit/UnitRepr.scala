package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.InfoBase.InfoBase

trait UnitRepr[Info[_], Repr[_]] extends InfoBase[Info, Repr] {
  def mkUnit: Repr[Unit]

  implicit def unitInfo: Info[Unit]
}
