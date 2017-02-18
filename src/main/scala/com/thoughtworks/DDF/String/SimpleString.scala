package com.thoughtworks.DDF.String

import com.thoughtworks.DDF.Arrow.SimpleArr
import com.thoughtworks.DDF.NoInfo

trait SimpleString[Repr[_]] extends StringType[NoInfo, Repr] with SimpleArr[Repr] {
  override def stringInfo: NoInfo[scala.Predef.String] = ???
}

object SimpleString {
  implicit def apply[Repr[_]] = new SimpleString[Repr] {}
}