package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.{EvalArrow, ArrowLoss}
import com.thoughtworks.DDF.{Eval, EvalCase, Loss, LossCase}

import scalaz.Leibniz._
import scalaz.Monoid

trait EvalProd extends ProdRepr[Loss, Eval] with EvalArrow {
  def peval[A, B](ab: Eval[(A, B)]): (Eval[A], Eval[B]) = witness(ab.ec.unique(PairEC[A, B]()))(ab.eca)

  def pairEval[A, B](a: Eval[A], b: Eval[B])(implicit al: Loss[A], bl: Loss[B]) = new Eval[(A, B)] {
    override val loss: Loss[(A, B)] = ProdInfo(al, bl)

    override def eval: (A, B) = (a.eval, b.eval)

    override val ec: EvalCase.Aux[(A, B), (Eval[A], Eval[B])] = PairEC()

    override def eca: ec.ret = (a, b)
  }

  override def fst[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[((A, B)) => A] =
    arrEval[(A, B), A, (at.loss, bt.loss), at.loss](p => (peval(p)._1, al => (al, bt.m.zero)))(ProdInfo(at, bt), at)

  override def snd[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[((A, B)) => B] =
    arrEval[(A, B), B, (at.loss, bt.loss), bt.loss](p => (peval(p)._2, bl => (at.m.zero, bl)))(ProdInfo(at, bt), bt)

  override def mkProd[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[(A) => (B) => (A, B)] =
    arrEval[A, B => (A, B), at.loss, ArrowLoss[B, (at.loss, bt.loss)]](a =>
      (arrEval[B, (A, B), bt.loss, (at.loss, bt.loss)](b =>
        (pairEval(a, b), _._2))(bt, ProdInfo(at, bt)),
        _.seq.map(_._2._1).foldRight[at.loss](at.m.zero)((x, y) => at.m.append(x, y))))(
      at, ArrInfo(bt, ProdInfo(at, bt)))

  override implicit def ProdInfo[A, B](implicit al: Loss[A], bl: Loss[B]): Loss.Aux[(A, B), (al.loss, bl.loss)] =
    new Loss[(A, B)] {
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

  override def ProdFstInfo[A, B]: Loss[(A, B)] => Loss[A] = p => witness(p.lc.unique(PairLC[A, B]()))(p.lca).Fst

  override def ProdSndInfo[A, B]: Loss[(A, B)] => Loss[B] = p => witness(p.lc.unique(PairLC[A, B]()))(p.lca).Snd

  def curry[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]): Eval[(((A, B)) => C) => A => B => C] =
    arrEval[((A, B)) => C, A => B => C, ArrowLoss[(A, B), ci.loss], ArrowLoss[A, ArrowLoss[B, ci.loss]]](abc =>
      (arrEval[A, B => C, ai.loss, ArrowLoss[B, ci.loss]](a =>
        (arrEval[B, C, bi.loss, ci.loss](b => {
          val c = aeval(abc).forward(pairEval(a, b))
          (c.eb, l => c.backward(l)._2)
        })(bi, ci), l => l.seq.map(x => {
          val c = aeval(abc).forward(pairEval(a, x._1))
          c.backward(x._2)._1
        }).foldRight(ai.m.zero)((x, y) => ai.m.append(x, y))))
        (ai, ArrInfo(bi, ci)), l => ArrowLoss(l.seq.flatMap(x => x._2.seq.map(y => (pairEval(x._1, y._1), y._2))))))

  def uncurry[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]): Eval[(A => B => C) => ((A, B)) => C] =
    arrEval[A => B => C, ((A, B)) => C, ArrowLoss[A, ArrowLoss[B, ci.loss]], ArrowLoss[(A, B), ci.loss]](abc =>
      (arrEval[(A, B), C, (ai.loss, bi.loss), ci.loss](ab => {
        val bc = aeval(abc).forward(peval(ab)._1)
        val c = aeval(bc.eb).forward(peval(ab)._2)
        (c.eb, l => (bc.backward(ArrowLoss(Seq((peval(ab)._2, l)))), c.backward(l)))
      })(ProdInfo(ai, bi), ci), l => ArrowLoss(l.seq.map(x => (peval(x._1)._1, ArrowLoss(Seq((peval(x._1)._2, x._2))))))))
}
