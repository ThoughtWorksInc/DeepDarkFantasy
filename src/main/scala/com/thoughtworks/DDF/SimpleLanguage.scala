package com.thoughtworks.DDF

import scala.language.higherKinds
object SimpleLanguage {

  final case class NoInfo[X]()

  implicit def noInfo[X]: NoInfo[X] = NoInfo[X]()

  trait SimpleLanguage[Repr[_]] extends Language[NoInfo, Repr] {
    override def Arr[A, B]: NoInfo[A] => NoInfo[B] => NoInfo[A => B] = _ => _ => noInfo

    override def ArrDom[A, B]: NoInfo[A => B] => NoInfo[A] = _ => noInfo

    override def ArrRng[A, B]: NoInfo[A => B] => NoInfo[B] = _ => noInfo

    override def ReprType[A]: Repr[A] => NoInfo[A] = _ => noInfo

    override def Pair[A, B]: (NoInfo[A]) => (NoInfo[B]) => NoInfo[(A, B)] = _ => _ => noInfo

    override def PairFst[A, B]: (NoInfo[(A, B)]) => NoInfo[A] = _ => noInfo

    override def PairSnd[A, B]: (NoInfo[(A, B)]) => NoInfo[B] = _ => noInfo
  }

}