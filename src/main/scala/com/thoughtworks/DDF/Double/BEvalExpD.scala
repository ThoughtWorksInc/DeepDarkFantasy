package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalExpD extends ExpD[LossInfo, BEval] with BEvalLitD {
  override def expD = aEval[scala.Double, scala.Double](x =>
    (litD(Math.exp(deval(x))), l => lossD(dloss(l) * Math.exp(deval(x)))))
}

object BEvalExpD {
  implicit def apply: BEvalExpD = new BEvalExpD { }
}
