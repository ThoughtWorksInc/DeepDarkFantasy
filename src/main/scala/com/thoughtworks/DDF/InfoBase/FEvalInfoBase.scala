package com.thoughtworks.DDF.InfoBase

import com.thoughtworks.DDF.{FEval, FEvalCase}

trait FEvalInfoBase extends InfoBase[FEvalCase, FEval] {
  override def reprInfo[A]: FEval[A] => FEvalCase[A] = _.fec
}

object FEvalInfoBase extends FEvalInfoBase