package com.thoughtworks.DDF.InfoBase

import com.thoughtworks.DDF.{ADEval, ADEvalCase}

trait ADEvalInfoBase extends InfoBase[ADEvalCase, ADEval] {
  override def reprInfo[A]: ADEval[A] => ADEvalCase[A] = _.fec
}

object ADEvalInfoBase extends ADEvalInfoBase