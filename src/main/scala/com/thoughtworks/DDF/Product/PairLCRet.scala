package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Eval.Loss

trait PairLCRet[A, B] {
  def Fst: Loss[A]

  def Snd: Loss[B]
}
