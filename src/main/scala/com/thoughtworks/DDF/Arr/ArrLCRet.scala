package com.thoughtworks.DDF.Arr

import com.thoughtworks.DDF.Eval.Loss

trait ArrLCRet[A, B] {
  def Dom: Loss[A]

  def Rng: Loss[B]
}
