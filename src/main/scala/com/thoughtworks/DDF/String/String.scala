package com.thoughtworks.DDF.String

import com.thoughtworks.DDF.Arrow.Arr

trait String extends StringType with Arr {
  def litString: scala.Predef.String => Repr[scala.Predef.String]

  def stringApp: Repr[scala.Predef.String => scala.Predef.String => scala.Predef.String]

  def stringApp_ : Repr[scala.Predef.String] => Repr[scala.Predef.String => scala.Predef.String] = app(stringApp)

  def stringApp__ : Repr[scala.Predef.String] => Repr[scala.Predef.String] => Repr[scala.Predef.String] = l =>
    app(stringApp_(l))
}
