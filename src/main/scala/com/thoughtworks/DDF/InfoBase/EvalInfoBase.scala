package com.thoughtworks.DDF.InfoBase

import com.thoughtworks.DDF.{Eval, Loss}

trait EvalInfoBase extends InfoBase[Loss, Eval] {
  override def reprInfo[A]: Eval[A] => Loss[A] = _.loss
}
