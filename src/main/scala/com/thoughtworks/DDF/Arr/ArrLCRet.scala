package com.thoughtworks.DDF.Arr

import com.thoughtworks.DDF.Loss

trait ArrLCRet[A, B] {
  def Dom: Loss[A]

  def Rng: Loss[B]
}
