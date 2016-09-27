package com.thoughtworks.DDF.Arr

import com.thoughtworks.DDF.{Eval, EvalCase, Loss, LossCase}
import com.thoughtworks.DDF.RI.RIEval

import scalaz.Leibniz._
import scalaz.Monoid

trait ArrEval extends ArrLang[Loss, Eval] with RIEval {
  def arrEval[A, B, AL, BL](f: Eval[A] => (Eval[B], BL => AL))(implicit al: Loss.Aux[A, AL], bl: Loss.Aux[B, BL]) =
    new Eval[A => B] {
      override val loss: Loss[A => B] = ArrInfo

      override def eval: A => B = a => eca.forward(al.conv(a)).eb.eval

      override val ec: EvalCase.Aux[A => B, forward[A, B]] = ArrEC[A, B]()

      override def eca: ec.ret = new forward[A, B] {
        override def forward(ea: Eval[A])(implicit al1: Loss[A], bl1: Loss[B]): backward[al1.loss, bl1.loss] =
          new backward[al1.loss, bl1.loss] {
            lazy val fea = f(ea)

            override lazy val eb: Eval[B] = fea._1

            override def backward: bl1.loss => al1.loss = bl2 => witness(al.unique(al1))(fea._2(witness(bl1.unique(bl))(bl2)))
          }
      }
    }

  override implicit def ArrInfo[A, B](implicit ai: Loss[A], bi: Loss[B]): Loss.Aux[A => B, ArrLoss[A, bi.loss]] =
    new Loss[A => B] {
      override type ret = ArrLoss[A, bi.loss]

      override def m: Monoid[loss] = new Monoid[loss] {
        override def zero: loss = ArrLoss(Seq())

        override def append(f1: loss, f2: => loss): loss = ArrLoss(f1.seq ++ f2.seq)
      }

      override def conv: (A => B) => Eval[A => B] = ab => arrEval[A, B, ai.loss, bi.loss](a =>
        (bi.conv(ab(a.eval)), _ => ai.m.zero))(ai, bi)

      override val lc: LossCase.Aux[A => B, ArrLCRet[A, B]] = ArrLC()

      override def lca: lc.ret = new ArrLCRet[A, B] {
        override def Dom: Loss[A] = ai

        override def Rng: Loss[B] = bi
      }
    }

  def aeval[A, B](ab: Eval[A => B]): forward[A, B] = witness(ab.ec.unique(ArrEC[A, B]()))(ab.eca)

  override def ArrDomInfo[A, B]: Loss[A => B] => Loss[A] = l => witness(l.lc.unique(ArrLC[A, B]()))(l.lca).Dom

  override def ArrRngInfo[A, B]: Loss[A => B] => Loss[B] = l => witness(l.lc.unique(ArrLC[A, B]()))(l.lca).Rng

  override def app[A, B] = f => x => aeval(f).forward(x)(ArrDomInfo(ReprInfo(f)), ArrRngInfo(ReprInfo(f))).eb
}
