package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.InfoBase.InfoBase

trait TopInfo[Info[_], Repr[_]] extends InfoBase[Info, Repr] {
  implicit def topInfo: Info[Unit]
}
