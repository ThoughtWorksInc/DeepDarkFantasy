package com.thoughtworks.DDF.InfoBase

import com.thoughtworks.DDF.{BEval, Loss}

trait BEvalInfoBase extends InfoBase[Loss, BEval] {
  override def reprInfo[A]: BEval[A] => Loss[A] = _.loss
}
