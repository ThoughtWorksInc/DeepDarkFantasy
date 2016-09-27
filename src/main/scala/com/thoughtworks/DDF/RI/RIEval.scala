package com.thoughtworks.DDF.RI

import com.thoughtworks.DDF.Eval._

trait RIEval extends RILang[Loss, Eval] {
  override def ReprInfo[A]: Eval[A] => Loss[A] = _.loss
}
