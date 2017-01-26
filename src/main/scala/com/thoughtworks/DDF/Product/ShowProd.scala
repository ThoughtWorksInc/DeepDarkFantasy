package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.{NoInfo, ShowLeaf}

trait ShowProd extends Prod[NoInfo, ShowLeaf] with ShowArr with SimpleProd[ShowLeaf] {
  override def mkProd[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = ShowLeaf("mkPair")

  override def fst[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = ShowLeaf("snd")

  override def zro[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = ShowLeaf("fst")

  override def curry[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = ShowLeaf("curry")

  override def uncurry[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = ShowLeaf("uncurry")

  override def ><[A, B, C, D](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C], di: NoInfo[D]) = ShowLeaf("><")
}

object ShowProd {
  implicit def apply = new ShowProd {}
}