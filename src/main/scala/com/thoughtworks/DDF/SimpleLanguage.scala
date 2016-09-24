package com.thoughtworks.DDF

import scala.language.higherKinds
object SimpleLanguage {

  final case class NoInfo[X]()

  implicit def noInfo[X]: NoInfo[X] = NoInfo[X]()

  trait SimpleLanguage[Repr[_]] extends Language[NoInfo, Repr] {
    override def ArrInfo[A, B]: NoInfo[A] => NoInfo[B] => NoInfo[A => B] = _ => _ => noInfo

    override def ArrDomInfo[A, B]: NoInfo[A => B] => NoInfo[A] = _ => noInfo

    override def ArrRngInfo[A, B]: NoInfo[A => B] => NoInfo[B] = _ => noInfo

    override def ReprInfo[A]: Repr[A] => NoInfo[A] = _ => noInfo

    override def PairInfo[A, B]: NoInfo[A] => NoInfo[B] => NoInfo[(A, B)] = _ => _ => noInfo

    override def PairFstInfo[A, B]: NoInfo[(A, B)] => NoInfo[A] = _ => noInfo

    override def PairSndInfo[A, B]: NoInfo[(A, B)] => NoInfo[B] = _ => noInfo

    override def DoubleInfo: NoInfo[Double] = noInfo
  }

}