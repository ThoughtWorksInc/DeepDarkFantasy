package com.thoughtworks.DDF

trait SimpleLang[Repr[_]] extends Lang[NoInfo, Repr] {
  implicit def noInfo[X]: NoInfo[X] = NoInfo[X]()

  override def ArrInfo[A, B]: NoInfo[A] => NoInfo[B] => NoInfo[A => B] = _ => _ => noInfo

  override def ArrDomInfo[A, B]: NoInfo[A => B] => NoInfo[A] = _ => noInfo

  override def ArrRngInfo[A, B]: NoInfo[A => B] => NoInfo[B] = _ => noInfo

  override def ReprInfo[A]: Repr[A] => NoInfo[A] = _ => noInfo

  override def ProdInfo[A, B]: NoInfo[A] => NoInfo[B] => NoInfo[(A, B)] = _ => _ => noInfo

  override def ProdFstInfo[A, B]: NoInfo[(A, B)] => NoInfo[A] = _ => noInfo

  override def ProdSndInfo[A, B]: NoInfo[(A, B)] => NoInfo[B] = _ => noInfo

  override def DoubleInfo: NoInfo[Double] = noInfo

  override def SumInfo[A, B]: NoInfo[A] => NoInfo[B] => NoInfo[Either[A, B]] = _ => _ => noInfo

  override def SumLeftInfo[A, B]: NoInfo[Either[A, B]] => NoInfo[A] = _ => noInfo

  override def SumRightInfo[A, B]: NoInfo[Either[A, B]] => NoInfo[B] = _ => noInfo

}