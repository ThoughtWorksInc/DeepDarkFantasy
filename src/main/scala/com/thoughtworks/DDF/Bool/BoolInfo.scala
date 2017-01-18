package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.InfoBase.InfoBase

trait BoolInfo[Info[_], Repr[_]] extends InfoBase[Info, Repr] {
  implicit def boolInfo: Info[Boolean]
}
