package com.thoughtworks.DDF.String

import com.thoughtworks.DDF.InfoBase.InfoBase

/**
  * Created by bill on 1/5/17.
  */
trait StringInfo[Info[_], Repr[_]] extends InfoBase[Info, Repr] {
  def stringInfo: Info[scala.Predef.String]
}

trait String[Info[_], Repr[_]] extends StringInfo[Info, Repr] {
  def litString: scala.Predef.String => Repr[scala.Predef.String]
}
