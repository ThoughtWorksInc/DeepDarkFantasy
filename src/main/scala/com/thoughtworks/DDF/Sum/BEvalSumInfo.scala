package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.BEvalArrowInfo
import com.thoughtworks.DDF.{BEval, BEvalMatch, CommutativeMonoid, Loss, LossMatch, LossInfo}

trait BEvalSumInfo extends SumInfo[LossInfo, BEval] with BEvalArrowInfo {
  trait SumLCRet[A, B] {
    def Left: LossInfo[A]

    def Right: LossInfo[B]
  }

  case class SumBEC[A, B]() extends BEvalMatch[Either[A, B]] {
    override type ret = Either[BEval[A], BEval[B]]
  }

  case class SumLC[A, B]() extends LossMatch[Either[A, B]] {
    override type ret = SumLCRet[A, B]
  }

  def lossS[A, B]: Loss[A] => Loss[B] => Loss[Either[A, B]] = l => r => new Loss[Either[A, B]] {
    override val tm: LossInfo.Aux[Either[A, B], (Loss[A], Loss[B])] = sumInfo(l.tm, r.tm)

    override val tmr: tm.loss = (l, r)
  }

  def sloss[A, B]: Loss[Either[A, B]] => (Loss[A], Loss[B]) = l => l.get(sumInfo(sumLeftInfo(l.tm), sumRightInfo(l.tm)))

  def slossl[A, B]: Loss[Either[A, B]] => Loss[A] = l => sloss(l)._1

  def slossr[A, B]: Loss[Either[A, B]] => Loss[B] = l => sloss(l)._2

  override implicit def sumInfo[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]):
  LossInfo.Aux[Either[A, B], (Loss[A], Loss[B])] =
    new LossInfo[Either[A, B]] {

      override def convert: Either[A, B] => BEval[Either[A, B]] = {
        case Left(x) => sumEval(scala.Left(ai.convert(x)))
        case Right(x) => sumEval(scala.Right(bi.convert(x)))
      }

      override val tm: LossMatch.Aux[Either[A, B], SumLCRet[A, B]] = SumLC()

      override val tmr: tm.ret = new SumLCRet[A, B] {
        override def Left: LossInfo[A] = ai

        override def Right: LossInfo[B] = bi
      }

      override type ret = (Loss[A], Loss[B])

      override def m: CommutativeMonoid[(Loss[A], Loss[B])] = new CommutativeMonoid[(Loss[A], Loss[B])] {
        override def zero: (Loss[A], Loss[B]) = (ai.lm.zero, bi.lm.zero)

        override def append(f1: (Loss[A], Loss[B]), f2: => (Loss[A], Loss[B])): (Loss[A], Loss[B]) =
          (ai.lm.append(f1._1, f2._1), bi.lm.append(f1._2, f2._2))
      }

      override def update(x: Either[A, B])(rate: Double)(l: (Loss[A], Loss[B])): Either[A, B] =
        x.left.map(y => ai.updatel(y)(rate)(l._1)).right.map(y => bi.updatel(y)(rate)(l._2))
    }

  override def sumLeftInfo[A, B]: LossInfo[Either[A, B]] => LossInfo[A] = _.get(SumLC[A, B]()).Left

  override def sumRightInfo[A, B]: LossInfo[Either[A, B]] => LossInfo[B] = _.get(SumLC[A, B]()).Right

  def sumEval[A, B](s: Either[BEval[A], BEval[B]])(implicit al: LossInfo[A], bl: LossInfo[B]) =
    new BEval[Either[A, B]] {
      override val loss: LossInfo[Either[A, B]] = sumInfo[A, B]

      override def eval: Either[A, B] = s match {
        case Left(x) => scala.Left(x.eval)
        case Right(x) => scala.Right(x.eval)
      }

      override val tm: BEvalMatch.Aux[Either[A, B], Either[BEval[A], BEval[B]]] = SumBEC()

      override val tmr: tm.ret = s
  }

  def seval[A, B](s: BEval[Either[A, B]]): Either[BEval[A], BEval[B]] = s.get(SumBEC[A, B]())
}

object BEvalSumInfo {
  implicit def apply: BEvalSumInfo = new BEvalSumInfo {}
}
