package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.NoInfo

trait SimpleSum[Repr[_]] extends SumLang[NoInfo, Repr] {
  override implicit def SumInfo[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): NoInfo[Either[A, B]] = ???
  override def SumLeftInfo[A, B]  = _ => NoInfo()

  override def SumRightInfo[A, B] = _ => NoInfo()
}
