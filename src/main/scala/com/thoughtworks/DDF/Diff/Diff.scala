package com.thoughtworks.DDF.Diff

import com.thoughtworks.DDF.ADEvalCase
import com.thoughtworks.DDF.Arrow.Arr
import com.thoughtworks.DDF.Gradient.Gradient

trait Diff[Info[_], Repr[_]] extends Arr[Info, Repr] {
  def diff[G: Gradient, A] : Repr[A] => Repr[fec.WithGrad[G]] forSome { val fec : ADEvalCase[A] }
}
