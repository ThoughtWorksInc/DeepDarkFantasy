package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arr.{ArrEval, ArrLoss}
import com.thoughtworks.DDF.{Eval, EvalCase, Loss, LossCase}

import scalaz.Leibniz._
import scalaz.Monoid

trait ProdEval extends ProdLang[Loss, Eval] with ArrEval {
  def peval[A, B](ab: Eval[(A, B)]): (Eval[A], Eval[B]) = witness(ab.ec.unique(PairEC[A, B]()))(ab.eca)

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

  def pairEval[A, B](a: Eval[A], b: Eval[B])(implicit al: Loss[A], bl: Loss[B]) = new Eval[(A, B)] {
    override val loss: Loss[(A, B)] = pairLoss(al, bl)

    override def eval: (A, B) = (a.eval, b.eval)

    override val ec: EvalCase.Aux[(A, B), (Eval[A], Eval[B])] = PairEC()

    override def eca: ec.ret = (a, b)
  }

  override def fst[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[((A, B)) => A] =
    arrEval[(A, B), A, (at.loss, bt.loss), at.loss](p => (peval(p)._1, al => (al, bt.m.zero)))(pairLoss(at, bt), at)

  override def snd[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[((A, B)) => B] =
    arrEval[(A, B), B, (at.loss, bt.loss), bt.loss](p => (peval(p)._2, bl => (at.m.zero, bl)))(pairLoss(at, bt), bt)

  override def mkProd[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[(A) => (B) => (A, B)] =
    arrEval[A, B => (A, B), at.loss, ArrLoss[B, (at.loss, bt.loss)]](a => (arrEval[B, (A, B), bt.loss, (at.loss, bt.loss)](b =>
      (pairEval(a, b), _._2))(bt, pairLoss(at, bt)), _.seq.map(_._2._1).foldRight[at.loss](at.m.zero)((x, y) => at.m.append(x, y))))(
      at, ArrInfo(bt, pairLoss(at, bt)))

  override def ProdInfo[A, B]: Loss[A] => Loss[B] => Loss[(A, B)] = a => b => pairLoss(a, b)

  override def ProdFstInfo[A, B]: Loss[(A, B)] => Loss[A] = p => witness(p.lc.unique(PairLC[A, B]()))(p.lca).Fst

  override def ProdSndInfo[A, B]: Loss[(A, B)] => Loss[B] = p => witness(p.lc.unique(PairLC[A, B]()))(p.lca).Snd
}
