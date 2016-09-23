package com.thoughtworks.DDF

import scala.language.higherKinds
object SimpleLanguage {

  final case class NoTypeInfo[X]()

  implicit def noTypeInfo[X]: NoTypeInfo[X] = NoTypeInfo[X]()

  trait SimpleLanguage[Repr[_]] extends Language[NoTypeInfo, Repr] {
    override def Arr[A, B]: NoTypeInfo[A] => NoTypeInfo[B] => NoTypeInfo[A => B] = _ => _ => NoTypeInfo()

    override def ArrDom[A, B]: NoTypeInfo[A => B] => NoTypeInfo[A] = _ => NoTypeInfo()

    override def ArrRng[A, B]: NoTypeInfo[A => B] => NoTypeInfo[B] = _ => NoTypeInfo()

    override def ReprType[A]: Repr[A] => NoTypeInfo[A] = _ => NoTypeInfo()

    override def Pair[A, B]: (NoTypeInfo[A]) => (NoTypeInfo[B]) => NoTypeInfo[(A, B)] = _ => _ => NoTypeInfo()

    override def PairFst[A, B]: (NoTypeInfo[(A, B)]) => NoTypeInfo[A] = _ => NoTypeInfo()

    override def PairSnd[A, B]: (NoTypeInfo[(A, B)]) => NoTypeInfo[B] = _ => NoTypeInfo()
  }

}