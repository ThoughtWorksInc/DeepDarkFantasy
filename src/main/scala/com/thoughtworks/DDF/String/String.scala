package com.thoughtworks.DDF.String

trait String[Info[_], Repr[_]] extends StringInfo[Info, Repr] {
  def litString: scala.Predef.String => Repr[scala.Predef.String]
}
