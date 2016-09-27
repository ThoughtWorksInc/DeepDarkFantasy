package com.thoughtworks.DDF.Pair

import com.thoughtworks.DDF.Eval._

import scalaz.Leibniz._
import scalaz.Monoid

object PairEval {
  case class PairEC[A, B]() extends EvalCase[(A, B)] {
    override type ret = (Eval[A], Eval[B])
  }

  def pairEval[A, B](a: Eval[A], b: Eval[B])(implicit al: Loss[A], bl: Loss[B]) = new Eval[(A, B)] {
    override val loss: Loss[(A, B)] = pairLoss(al, bl)

    override def eval: (A, B) = (a.eval, b.eval)

    override val ec: EvalCase.Aux[(A, B), (Eval[A], Eval[B])] = PairEC()

    override def eca: ec.ret = (a, b)
  }

  trait PairLCRet[A, B] {
    def Fst: Loss[A]

    def Snd: Loss[B]
  }

  case class PairLC[A, B]() extends LossCase[(A, B)] {
    override type ret = PairLCRet[A, B]
  }

  implicit def pairLoss[A, B](implicit al: Loss[A], bl: Loss[B]): Loss.Aux[(A, B), (al.loss, bl.loss)] = new Loss[(A, B)] {

    override def conv: ((A, B)) => Eval[(A, B)] = p => pairEval(al.conv(p._1), bl.conv(p._2))

    override val lc: LossCase.Aux[(A, B), PairLCRet[A, B]] = PairLC[A, B]()

    override def lca: lc.ret = new PairLCRet[A, B] {
      override def Fst: Loss[A] = al

      override def Snd: Loss[B] = bl
    }

    override type ret = (al.loss, bl.loss)

    override def m: Monoid[(al.loss, bl.loss)] = new Monoid[(al.loss, bl.loss)] {
      override def zero: (al.loss, bl.loss) = (al.m.zero, bl.m.zero)

      override def append(f1: (al.loss, bl.loss), f2: => (al.loss, bl.loss)): (al.loss, bl.loss) =
        (al.m.append(f1._1, f2._1), bl.m.append(f1._2, f2._2))
    }
  }

  def peval[A, B](ab: Eval[(A, B)]): (Eval[A], Eval[B]) = witness(ab.ec.unique(PairEC[A, B]()))(ab.eca)
}
