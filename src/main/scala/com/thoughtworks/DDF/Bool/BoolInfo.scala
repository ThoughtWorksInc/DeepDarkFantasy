package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.InfoB.InfoB

trait BoolInfo[Info[_], Repr[_]] extends InfoB[Info, Repr] {
  implicit def BoolInfo: Info[Boolean]
}
