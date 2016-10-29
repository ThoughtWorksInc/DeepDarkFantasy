package com.thoughtworks.DDF.InfoBase

import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalInfoBase extends InfoBase[LossInfo, BEval] {
  override def reprInfo[A]: BEval[A] => LossInfo[A] = _.loss
}
