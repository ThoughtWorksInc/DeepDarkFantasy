package com.thoughtworks.DDF.Diff

import com.thoughtworks.DDF.ADEvalCase
import com.thoughtworks.DDF.Arrow.Arr
import com.thoughtworks.DDF.Gradient.Gradient

trait Diff[Info[_], Repr[_]] extends Arr[Info, Repr] {
  def diff[G: Gradient, A](implicit ai: Info[A]) : Repr[A => ADEvalCase[A]#WithGrad[G]]
}
