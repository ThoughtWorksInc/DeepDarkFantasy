package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.InfoBase.InfoBase

trait UnitInfo[Info[_], Repr[_]] extends InfoBase[Info, Repr] {
  implicit def unitInfo: Info[Unit]
}
