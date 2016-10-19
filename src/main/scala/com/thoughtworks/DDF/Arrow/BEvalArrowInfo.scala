package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.InfoBase.BEvalInfoBase
import com.thoughtworks.DDF.{BEval, BEvalCase, CommutativeMonoid, Loss, LossCase}
import scalaz.Leibniz.witness

trait ArrowLoss[A, BL] {
  def mapReduce[C](f: BEval[A] => BL => C)(implicit m: CommutativeMonoid[C]): C
}

object ArrowLoss {
  def apply[A, BL](eva: BEval[A])(bl: BL) = new ArrowLoss[A, BL] {
    override def mapReduce[C](f: BEval[A] => BL => C)(implicit m: CommutativeMonoid[C]): C = f(eva)(bl)
  }
}

trait BEvalArrowInfo extends ArrowInfo[Loss, BEval] with BEvalInfoBase {
  case class ArrowBEC[A, B]() extends BEvalCase[A => B] {
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
      val eb: BEval[B]

      def backward: BL => AL
    }

    def forward(ea: BEval[A])(implicit al: Loss[A], bl: Loss[B]): backward[al.loss, bl.loss]
  }

  def arrowEval[A, B, AL, BL](f: BEval[A] => (BEval[B], BL => AL))(implicit al: Loss.Aux[A, AL], bl: Loss.Aux[B, BL]):
  BEval[A => B] =
    new BEval[A => B] {
      override val loss: Loss[A => B] = arrowInfo

      override def eval: A => B = a => eca.forward(al.convert(a)).eb.eval

      override val ec: BEvalCase.Aux[A => B, forward[A, B]] = ArrowBEC[A, B]()

      override def eca: ec.ret = new forward[A, B] {
        override def forward(ea: BEval[A])(implicit al1: Loss[A], bl1: Loss[B]): backward[al1.loss, bl1.loss] =
          new backward[al1.loss, bl1.loss] {
            lazy val fea = f(ea)

            override lazy val eb: BEval[B] = fea._1

            override def backward: bl1.loss => al1.loss = bl2 => witness(al.unique(al1))(fea._2(witness(bl1.unique(bl))(bl2)))
          }
      }
    }

  override implicit def arrowInfo[A, B](implicit ai: Loss[A], bi: Loss[B]): Loss.Aux[A => B, ArrowLoss[A, bi.loss]] =
    new Loss[A => B] {
      override type ret = ArrowLoss[A, bi.loss]

      override def m: CommutativeMonoid[loss] = new CommutativeMonoid[loss] {
        override def zero: loss = new ArrowLoss[A, bi.loss] {
          override def mapReduce[C](f: BEval[A] => bi.loss => C)(implicit m: CommutativeMonoid[C]): C = m.zero
        }

        override def append(f1: loss, f2: => loss): loss = new ArrowLoss[A, bi.loss] {
          override def mapReduce[C](f: BEval[A] => bi.loss => C)(implicit m: CommutativeMonoid[C]): C =
            m.append(f1.mapReduce(f), f2.mapReduce(f))
        }
      }

      override def convert: (A => B) => BEval[A => B] = ab => arrowEval[A, B, ai.loss, bi.loss](a =>
        (bi.convert(ab(a.eval)), _ => ai.m.zero))(ai, bi)

      override val lc: LossCase.Aux[A => B, ArrowLCRet[A, B]] = ArrowLC()

      override def lca: lc.ret = new ArrowLCRet[A, B] {
        override def Dom: Loss[A] = ai

        override def Rng: Loss[B] = bi
      }

      override def update(x: A => B)(rate: Double)(l: loss): A => B = x
    }

  def aeval[A, B](ab: BEval[A => B]): forward[A, B] = witness(ab.ec.unique(ArrowBEC[A, B]()))(ab.eca)

  override def arrowDomainInfo[A, B]: Loss[A => B] => Loss[A] = l => witness(l.lc.unique(ArrowLC[A, B]()))(l.lca).Dom

  override def arrowRangeInfo[A, B]: Loss[A => B] => Loss[B] = l => witness(l.lc.unique(ArrowLC[A, B]()))(l.lca).Rng
}

object BEvalArrowInfo {
  implicit def apply: BEvalArrowInfo = new BEvalArrowInfo {}
}
