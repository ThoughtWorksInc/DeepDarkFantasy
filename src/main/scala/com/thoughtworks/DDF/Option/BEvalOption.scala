package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.BEvalArr
import com.thoughtworks.DDF.Combinators.{BEvalComb, Comb}
import com.thoughtworks.DDF.{BEval, BEvalMatch, CommutativeMonoid, Loss, LossInfo, LossMatch}

trait BEvalOption extends Option[LossInfo, BEval] with BEvalArr {
  case class OptionLC[A]() extends LossMatch[scala.Option[A]] {
    type ret = LossInfo[A]
  }

  def lossO[A]: Loss[A] => Loss[scala.Option[A]] = l => new Loss[scala.Option[A]] {
    override val tm: LossInfo.Aux[scala.Option[A], Loss[A]] = optionInfo[A](l.tm)

    override val tmr: tm.ret = l
  }

  def oloss[A]: Loss[scala.Option[A]] => Loss[A] = l => l.get(optionInfo(optionElmInfo(l.tm)))

  override implicit def optionInfo[A](implicit ai: LossInfo[A]): LossInfo.Aux[scala.Option[A], Loss[A]] =
    new LossInfo[scala.Option[A]] {
      override def m: CommutativeMonoid[Loss[A]] = ai.lm

      override def convert: scala.Option[A] => BEval[scala.Option[A]] = x => optionEval[A](x.map(ai.convert))

      override type ret = Loss[A]

      override def update(x: scala.Option[A])(rate: Double)(l: Loss[A]): scala.Option[A] =
        x.map(y => ai.updatel(y)(rate)(l))

      override val tm: LossMatch.Aux[scala.Option[A], LossInfo[A]] = OptionLC()

      override val tmr: tm.ret = ai
    }

  case class OptionBEC[A]() extends BEvalMatch[scala.Option[A]] {
    type ret = scala.Option[BEval[A]]
  }

  def optionEval[A](opt: scala.Option[BEval[A]])(implicit ai: LossInfo[A]) = new BEval[scala.Option[A]] {
    override val loss: LossInfo[scala.Option[A]] = optionInfo(ai)

    override def eval: scala.Option[A] = opt.map(_.eval)

    override val tm: BEvalMatch.Aux[scala.Option[A], scala.Option[BEval[A]]] = OptionBEC()

    override val tmr: tm.ret = opt
  }

  def oeval[A](opt: BEval[scala.Option[A]]): scala.Option[BEval[A]] = opt.get(OptionBEC[A]())

  override def optionElmInfo[A]: LossInfo[scala.Option[A]] => LossInfo[A] = _.get(OptionLC[A]())

  override def none[A](implicit ai: LossInfo[A]): BEval[scala.Option[A]] = optionEval[A](None)

  override def some[A](implicit ai: LossInfo[A]): BEval[A => scala.Option[A]] =
    aEval[A, scala.Option[A]](a => (optionEval(Some(a)), oloss))(ai, optionInfo(ai))

  private def comb : Comb[LossInfo, BEval] = BEvalComb.apply

  override def optionMatch[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]):
  BEval[scala.Option[A] => B => (A => B) => B] =
    aEval[scala.Option[A], B => (A => B) => B](opt => {
      oeval(opt) match {
        case None => (comb.K[B, A => B], _ => optionInfo(ai).lm.zero)
        case Some(a) => (comb.K_[(A => B) => B, B](comb.Let_[A, B](a))(bi),
          x => aloss(x).mapReduce(b => y => aloss(y).mapReduce(ab => l => lossO(aeval(ab).forward(a).backward(l)))(
            optionInfo(ai).lm))(optionInfo(ai).lm))
      }
    })
}

object BEvalOption {
  implicit def apply = new BEvalOption {}
}