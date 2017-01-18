package com.thoughtworks.DDF.String

/**
  * Created by bill on 1/5/17.
  */
trait StringInfo[Info[_], Repr[_]] {
  def stringInfo: Info[scala.Predef.String]
}

trait String[Info[_], Repr[_]] extends StringInfo[Info, Repr] {
  def litString: scala.Predef.String => Repr[scala.Predef.String]
}
