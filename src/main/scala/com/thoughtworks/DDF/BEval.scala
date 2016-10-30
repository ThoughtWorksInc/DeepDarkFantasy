package com.thoughtworks.DDF

import scalaz.Leibniz._

trait LossInfo[X] extends TypeMatch[LossInfo, X] with TypeCase[LossMatch, X] { ext =>
  final type loss = ret

  def m: CommutativeMonoid[loss]

  def lm: CommutativeMonoid[Loss[X]] = new CommutativeMonoid[Loss[X]] {
    override def zero: Loss[X] = new Loss[X] {
      override val tm: LossInfo.Aux[X, ret] = ext

      override val tmr: tm.ret = m.zero
    }

    override def append(f1: Loss[X], f2: => Loss[X]): Loss[X] = new Loss[X] {
      val f2_ : Loss[X] = f2
      override val tm: LossInfo.Aux[X, ret] = ext

      override val tmr: tm.ret = m.append(f1.get(tm), f2.get(tm))
    }
  }
  def convert: /*lose backprop ability*/ X => BEval[X]

  def update(x: X)(rate: Double)(l: loss): X

  def updatel(x: X)(rate: Double)(l: Loss[X]): X = update(x)(rate)(l.get[loss](ext))
}

object LossInfo {
  type Aux[X, XL] = LossInfo[X] {type ret = XL}
}

trait LossMatch[X] extends TypeMatch[LossMatch, X]

object LossMatch {
  type Aux[X, Y] = LossMatch[X] {type ret = Y}
}

trait Loss[X] extends TypeCase[LossInfo, X]

object Loss {
  def apply[X, XL](l: XL)(implicit xi: LossInfo.Aux[X, XL]) = new Loss[X] {
    override val tm: LossInfo.Aux[X, XL] = xi

    override val tmr: tm.ret = l
  }
}

trait BEvalMatch[X] extends TypeMatch[BEvalMatch, X]

object BEvalMatch {
  type Aux[X, Y] = BEvalMatch[X] {type ret = Y}
}

trait BEval[X] extends TypeCase[BEvalMatch, X] {
  val loss: LossInfo[X]

  def eval: /*should not be used when defining instance of Eval*/ X
}