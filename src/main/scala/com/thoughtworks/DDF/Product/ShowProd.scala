package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arr.ShowArr
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowProd extends ProdLang[NoInfo, Show] with ShowArr with SimpleProd[Show] {
  override def mkProd[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("mkPair")

  override def snd[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("snd")

  override def fst[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("fst")

  override def curry[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = Show("curry")

  override def uncurry[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = Show("uncurry")
}

object ShowProd {
  implicit def apply = new ShowProd {}
}