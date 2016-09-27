package com.thoughtworks.DDF.RI

import com.thoughtworks.DDF.{Eval, Loss}

trait RIEval extends RILang[Loss, Eval] {
  override def ReprInfo[A]: Eval[A] => Loss[A] = _.loss
}
