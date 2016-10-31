package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.InfoBase.BEvalInfoBase
import com.thoughtworks.DDF.{BEval, BEvalMatch, CommutativeMonoid, Loss, LossMatch, LossInfo}

import scalaz.Leibniz.witness

trait ArrowLoss[A, B] {
  def mapReduce[C](f: BEval[A] => Loss[B] => C)(implicit m: CommutativeMonoid[C]): C
}

trait BEvalArrInfo extends ArrInfo[LossInfo, BEval] with BEvalInfoBase {
  def lossA[A, B]: BEval[A] => Loss[B] => Loss[A => B] = eva => bl => new Loss[A => B] {
    override val tm: LossInfo.Aux[A => B, ArrowLoss[A, B]] = aInfo[A, B](eva.loss, bl.tm)

    override val tmr: tm.loss = new ArrowLoss[A, B] {
      override def mapReduce[C](f: BEval[A] => Loss[B] => C)(implicit m: CommutativeMonoid[C]): C = f(eva)(bl)
    }
  }

  def aloss[A, B]: Loss[A => B] => ArrowLoss[A, B] = l => l.get(aInfo[A, B](domInfo(l.tm), rngInfo(l.tm)))

  case class ArrowBEC[A, B]() extends BEvalMatch[A => B] {
    override type ret = Forward[A, B]
  }

  case class ArrowLC[A, B]() extends LossMatch[A => B] {
    override type ret = ArrowLCRet[A, B]
  }

  trait ArrowLCRet[A, B] {
    def Dom: LossInfo[A]

    def Rng: LossInfo[B]
  }

  trait Forward[A, B] {

    trait Backward {
      val eb: BEval[B]

      def backward: Loss[B] => Loss[A]
    }

    def forward(ea: BEval[A]): Backward
  }

  def aEval[A, B](f: BEval[A] => (BEval[B], Loss[B] => Loss[A]))(implicit ai: LossInfo[A], bi: LossInfo[B]):
  BEval[A => B] =
    new BEval[A => B] {
      override val loss: LossInfo[A => B] = aInfo

      override def eval: A => B = a => tmr.forward(ai.convert(a)).eb.eval

      override val tm: BEvalMatch.Aux[A => B, Forward[A, B]] = ArrowBEC[A, B]()

      override val tmr: tm.ret = new Forward[A, B] {
        override def forward(ea: BEval[A]): Backward =
          new Backward {
            lazy val fea = f(ea)

            override lazy val eb: BEval[B] = fea._1

            override def backward: Loss[B] => Loss[A] = fea._2
          }
      }
    }

  override implicit def aInfo[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): LossInfo.Aux[A => B, ArrowLoss[A, B]] =
    new LossInfo[A => B] {
      override type ret = ArrowLoss[A, B]

      override def m: CommutativeMonoid[loss] = new CommutativeMonoid[loss] {
        override def zero: loss = new ArrowLoss[A, B] {
          override def mapReduce[C](f: BEval[A] => Loss[B] => C)(implicit m: CommutativeMonoid[C]): C = m.zero
        }

        override def append(f1: loss, f2: => loss): loss = new ArrowLoss[A, B] {
          override def mapReduce[C](f: BEval[A] => Loss[B] => C)(implicit m: CommutativeMonoid[C]): C =
            m.append(f1.mapReduce(f), f2.mapReduce(f))
        }
      }

      override def convert: (A => B) => BEval[A => B] = ab => aEval[A, B](a =>
        (bi.convert(ab(a.eval)), _ => Loss(ai.m.zero)(ai)))(ai, bi)

      override val tm: LossMatch.Aux[A => B, ArrowLCRet[A, B]] = ArrowLC()

      override val tmr: tm.ret = new ArrowLCRet[A, B] {
        override def Dom: LossInfo[A] = ai

        override def Rng: LossInfo[B] = bi
      }

      override def update(x: A => B)(rate: Double)(l: loss): A => B = x
    }

  def aeval[A, B]: BEval[A => B] => Forward[A, B] = _.get(ArrowBEC[A, B]())

  override def domInfo[A, B]: LossInfo[A => B] => LossInfo[A] = _.get(ArrowLC[A, B]()).Dom

  override def rngInfo[A, B]: LossInfo[A => B] => LossInfo[B] = _.get(ArrowLC[A, B]()).Rng
}

object BEvalArrInfo {
  implicit def apply: BEvalArrInfo = new BEvalArrInfo {}
}
