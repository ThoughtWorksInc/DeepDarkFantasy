package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.{BEval, Loss}

trait BEvalExpD extends ExpD[Loss, BEval] with BEvalLitD {
  override def expD = arrowEval[scala.Double, scala.Double, DLoss, DLoss](x =>
    (litD(Math.exp(deval(x))), l => DLoss(l.d * Math.exp(deval(x)))))
}

object BEvalExpD {
  implicit def apply: BEvalExpD = new BEvalExpD { }
}
