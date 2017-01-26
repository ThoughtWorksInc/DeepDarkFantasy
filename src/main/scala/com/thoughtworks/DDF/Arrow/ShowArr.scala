package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowArr extends Arr[NoInfo, Lambda[X => Show]] with SimpleArr[Lambda[X => Show]] {
  override def app[A, B] = f => x => Show(f)(x)
}

object ShowArr extends ShowArr