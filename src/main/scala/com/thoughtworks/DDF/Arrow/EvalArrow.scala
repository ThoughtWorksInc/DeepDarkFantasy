package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.{Eval, EvalCase, Loss, LossCase}
import com.thoughtworks.DDF.InfoBase.EvalInfoBase

import scalaz.Leibniz._
import scalaz.Monoid

case class ArrowLoss[A, BL](seq: Seq[(Eval[A], BL)])

trait EvalArrow extends ArrowRepr[Loss, Eval] with EvalInfoBase {
  case class ArrowEC[A, B]() extends EvalCase[A => B] {
    override type ret = forward[A, B]
  }

  case class ArrowLC[A, B]() extends LossCase[A => B] {
    override type ret = ArrowLCRet[A, B]
  }

  trait ArrowLCRet[A, B] {
    def Dom: Loss[A]

    def Rng: Loss[B]
  }

  trait forward[A, B] {

    trait backward[AL, BL] {
      val eb: Eval[B]

      def backward: BL => AL
    }

    def forward(ea: Eval[A])(implicit al: Loss[A], bl: Loss[B]): backward[al.loss, bl.loss]
  }

  def arrowEval[A, B, AL, BL](f: Eval[A] => (Eval[B], BL => AL))(implicit al: Loss.Aux[A, AL], bl: Loss.Aux[B, BL]) =
    new Eval[A => B] {
      override val loss: Loss[A => B] = arrowInfo

      override def eval: A => B = a => eca.forward(al.convert(a)).eb.eval

      override val ec: EvalCase.Aux[A => B, forward[A, B]] = ArrowEC[A, B]()

      override def eca: ec.ret = new forward[A, B] {
        override def forward(ea: Eval[A])(implicit al1: Loss[A], bl1: Loss[B]): backward[al1.loss, bl1.loss] =
          new backward[al1.loss, bl1.loss] {
            lazy val fea = f(ea)

            override lazy val eb: Eval[B] = fea._1

            override def backward: bl1.loss => al1.loss = bl2 => witness(al.unique(al1))(fea._2(witness(bl1.unique(bl))(bl2)))
          }
      }
    }

  override implicit def arrowInfo[A, B](implicit ai: Loss[A], bi: Loss[B]): Loss.Aux[A => B, ArrowLoss[A, bi.loss]] =
    new Loss[A => B] {
      override type ret = ArrowLoss[A, bi.loss]

      override def m: Monoid[loss] = new Monoid[loss] {
        override def zero: loss = ArrowLoss(Seq())

        override def append(f1: loss, f2: => loss): loss = ArrowLoss(f1.seq ++ f2.seq)
      }

      override def convert: (A => B) => Eval[A => B] = ab => arrowEval[A, B, ai.loss, bi.loss](a =>
        (bi.convert(ab(a.eval)), _ => ai.m.zero))(ai, bi)

      override val lc: LossCase.Aux[A => B, ArrowLCRet[A, B]] = ArrowLC()

      override def lca: lc.ret = new ArrowLCRet[A, B] {
        override def Dom: Loss[A] = ai

        override def Rng: Loss[B] = bi
      }
    }

  def aeval[A, B](ab: Eval[A => B]): forward[A, B] = witness(ab.ec.unique(ArrowEC[A, B]()))(ab.eca)

  override def arrowDomainInfo[A, B]: Loss[A => B] => Loss[A] = l => witness(l.lc.unique(ArrowLC[A, B]()))(l.lca).Dom

  override def arrowRangeInfo[A, B]: Loss[A => B] => Loss[B] = l => witness(l.lc.unique(ArrowLC[A, B]()))(l.lca).Rng

  override def app[A, B] = f => x => aeval(f).forward(x)(arrowDomainInfo(reprInfo(f)), arrowRangeInfo(reprInfo(f))).eb
}
