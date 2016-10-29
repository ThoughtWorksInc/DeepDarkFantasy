package com.thoughtworks.DDF

import scalaz.Leibniz._

trait LossInfo[X] extends TypeCase[LossInfo, X] { ext =>
  final type loss = ret

  def m: CommutativeMonoid[loss]

  def lm: CommutativeMonoid[Loss[X]] = new CommutativeMonoid[Loss[X]] {
    override def zero: Loss[X] = new Loss[X] {
      override val li: LossInfo.Aux[X, ret] = ext

      override val x: li.loss = m.zero
    }

    override def append(f1: Loss[X], f2: => Loss[X]): Loss[X] = new Loss[X] {
      val f2_ : Loss[X] = f2

      override val li: LossInfo.Aux[X, ret] = ext

      override val x: ext.loss = m.append(witness(f1.li.unique(li))(f1.x), witness(f2_.li.unique(li))(f2_.x))
    }
  }
  def convert: /*lose backprop ability*/ X => BEval[X]

  val lc: LossCase[X]

  def lca: lc.ret

  def update(x: X)(rate: Double)(l: loss): X

  def updatel(x: X)(rate: Double)(l: Loss[X]): X = update(x)(rate)(witness(l.li.unique(ext))(l.x))
}

object LossInfo {
  type Aux[X, XL] = LossInfo[X] {type ret = XL}
}

trait LossCase[X] extends TypeCase[LossCase, X]

object LossCase {
  type Aux[X, Y] = LossCase[X] {type ret = Y}
}

trait Loss[X] {
  val li: LossInfo[X]

  val x: li.loss
}

object Loss {
  def apply[X, XL](l: XL)(implicit xi: LossInfo.Aux[X, XL]) = new Loss[X] {
    override val li: LossInfo.Aux[X, XL] = xi

    override val x: li.loss = l
  }
}

trait BEvalCase[X] extends TypeCase[BEvalCase, X]

object BEvalCase {
  type Aux[X, Y] = BEvalCase[X] {type ret = Y}
}

trait BEval[X] {
  val loss: LossInfo[X]

  def eval: /*should not be used when defining instance of Eval*/ X

  val ec: BEvalCase[X]

  def eca: ec.ret
}