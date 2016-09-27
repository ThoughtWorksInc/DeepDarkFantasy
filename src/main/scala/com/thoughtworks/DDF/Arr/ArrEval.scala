package com.thoughtworks.DDF.Arr

import com.thoughtworks.DDF.Eval._
import scalaz.Leibniz._
import scalaz.Monoid

object ArrEval {
  trait ArrEval[A, B] {

    trait backward[AL, BL] {
      val eb: Eval[B]

      def backward: BL => AL
    }

    def forward(ea: Eval[A])(implicit al: Loss[A], bl: Loss[B]): backward[al.loss, bl.loss]
  }

  trait ArrLCRet[A, B] {
    def Dom: Loss[A]

    def Rng: Loss[B]
  }

  case class ArrLC[A, B]() extends LossCase[A => B] {
    override type ret = ArrLCRet[A, B]
  }

  case class ArrEC[A, B]() extends EvalCase[A => B] {
    override type ret = ArrEval[A, B]
  }

  def arrEval[A, B, AL, BL](f: Eval[A] => (Eval[B], BL => AL))(implicit al: Loss.Aux[A, AL], bl: Loss.Aux[B, BL]) =
    new Eval[A => B] {
      override val loss: Loss[A => B] = arrLoss(al, bl)

      override def eval: A => B = a => eca.forward(al.conv(a)).eb.eval

      override val ec: EvalCase.Aux[A => B, ArrEval[A, B]] = ArrEC[A, B]()

      override def eca: ec.ret = new ArrEval[A, B] {
        override def forward(ea: Eval[A])(implicit al1: Loss[A], bl1: Loss[B]): backward[al1.loss, bl1.loss] =
          new backward[al1.loss, bl1.loss] {
            lazy val fea = f(ea)

            override lazy val eb: Eval[B] = fea._1

            override def backward: bl1.loss => al1.loss = bl2 => witness(al.unique(al1))(fea._2(witness(bl1.unique(bl))(bl2)))
          }
      }
    }

  case class ArrLoss[A, BL](seq: Seq[(Eval[A], BL)])

  implicit def arrLoss[A, B](implicit al: Loss[A], bl: Loss[B]): Loss.Aux[A => B, ArrLoss[A, bl.loss]] = new Loss[A => B] {

    override type ret = ArrLoss[A, bl.loss]

    override def m: Monoid[loss] = new Monoid[loss] {
      override def zero: loss = ArrLoss(Seq())

      override def append(f1: loss, f2: => loss): loss = ArrLoss(f1.seq ++ f2.seq)
    }

    override def conv: (A => B) => Eval[A => B] = ab => arrEval[A, B, al.loss, bl.loss](a =>
      (bl.conv(ab(a.eval)), _ => al.m.zero))(al, bl)

    override val lc: LossCase.Aux[A => B, ArrLCRet[A, B]] = ArrLC()

    override def lca: lc.ret = new ArrLCRet[A, B] {
      override def Dom: Loss[A] = al

      override def Rng: Loss[B] = bl
    }
  }

  def aeval[A, B](ab: Eval[A => B]): ArrEval[A, B] = witness(ab.ec.unique(ArrEC[A, B]()))(ab.eca)
}
