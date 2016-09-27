package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Loss

trait SumLCRet[A, B] {
  def Left: Loss[A]

  def Right: Loss[B]
}
