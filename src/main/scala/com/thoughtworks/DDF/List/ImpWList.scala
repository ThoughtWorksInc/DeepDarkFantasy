package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.ImpWArrowMin
import com.thoughtworks.DDF.{ImpW, NoInfo}

trait ImpWList[Info[_], Repr[_]] extends List[NoInfo, ImpW[Info, Repr, ?]] with ImpWArrowMin[Info, Repr] {

}

object ImpWList {
  implicit def apply[Info[_], Repr[_]]: ImpWList[Info, Repr] = new ImpWList[Info, Repr] {

  }
}
