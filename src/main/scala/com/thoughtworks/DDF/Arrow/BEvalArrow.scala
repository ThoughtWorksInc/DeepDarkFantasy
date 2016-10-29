package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalArrow extends Arrow[LossInfo, BEval] with BEvalArrowInfo {
  override def app[A, B] = f => x => aeval(f).forward(x)(arrowDomainInfo(reprInfo(f)), arrowRangeInfo(reprInfo(f))).eb
}

object BEvalArrow {
  implicit def apply: BEvalArrow = new BEvalArrow {}
}