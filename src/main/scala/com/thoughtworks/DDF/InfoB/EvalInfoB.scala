package com.thoughtworks.DDF.InfoB

import com.thoughtworks.DDF.{Eval, Loss}

trait EvalInfoB extends InfoB[Loss, Eval] {
  override def ReprInfo[A]: Eval[A] => Loss[A] = _.loss
}
