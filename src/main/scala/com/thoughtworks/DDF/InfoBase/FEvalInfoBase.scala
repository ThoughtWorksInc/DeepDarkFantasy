package com.thoughtworks.DDF.InfoBase

import com.thoughtworks.DDF.{FEval, FEvalCase}

trait FEvalInfoBase[G] extends InfoBase[FEvalCase[G, ?], FEval[G, ?]] {
  override def reprInfo[A]: FEval[G, A] => FEvalCase[G, A] = _.tm
}

object FEvalInfoBase {
  implicit def apply[G]: FEvalInfoBase[G] = new FEvalInfoBase[G] { }
}