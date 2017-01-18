package com.thoughtworks.DDF.Bool

trait BoolInfo[Info[_], Repr[_]] {
  implicit def boolInfo: Info[Boolean]
}
