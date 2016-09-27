package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.{Eval, EvalCase}

case class SumEC[A, B]() extends EvalCase[Either[A, B]] {
  override type ret = Either[Eval[A], Eval[B]]
}
