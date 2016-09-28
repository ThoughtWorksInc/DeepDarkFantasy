package com.thoughtworks.DDF.Unit

trait UnitLang[Info[_], Repr[_]] {
  def mkUnit: Repr[Unit]

  implicit def UnitInfo: Info[Unit]
}
