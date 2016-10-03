package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.{EvalArrow, ArrowLoss}
import com.thoughtworks.DDF.{Eval, EvalCase, Loss, LossCase}

import scalaz.Leibniz._
import scalaz.Monoid

trait EvalProduct extends ProductRepr[Loss, Eval] with EvalArrow {
  trait ProductLCRet[A, B] {
    def Fst: Loss[A]

    def Snd: Loss[B]
  }

  case class ProductEC[A, B]() extends EvalCase[(A, B)] {
    override type ret = (Eval[A], Eval[B])
  }

  case class ProductLC[A, B]() extends LossCase[(A, B)] {
    override type ret = ProductLCRet[A, B]
  }

  def peval[A, B](ab: Eval[(A, B)]): (Eval[A], Eval[B]) = witness(ab.ec.unique(ProductEC[A, B]()))(ab.eca)

  def pairEval[A, B](a: Eval[A], b: Eval[B])(implicit al: Loss[A], bl: Loss[B]) = new Eval[(A, B)] {
    override val loss: Loss[(A, B)] = productInfo(al, bl)

    override def eval: (A, B) = (a.eval, b.eval)

    override val ec: EvalCase.Aux[(A, B), (Eval[A], Eval[B])] = ProductEC()

    override def eca: ec.ret = (a, b)
  }

  override def first[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[((A, B)) => A] =
    arrowEval[(A, B), A, (at.loss, bt.loss), at.loss](p => (peval(p)._1, al => (al, bt.m.zero)))(productInfo(at, bt), at)

  override def second[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[((A, B)) => B] =
    arrowEval[(A, B), B, (at.loss, bt.loss), bt.loss](p => (peval(p)._2, bl => (at.m.zero, bl)))(productInfo(at, bt), bt)

  override def mkProduct[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[(A) => (B) => (A, B)] =
    arrowEval[A, B => (A, B), at.loss, ArrowLoss[B, (at.loss, bt.loss)]](a =>
      (arrowEval[B, (A, B), bt.loss, (at.loss, bt.loss)](b =>
        (pairEval(a, b), _._2))(bt, productInfo(at, bt)),
        _.seq.map(_._2._1).foldRight[at.loss](at.m.zero)((x, y) => at.m.append(x, y))))(
      at, arrowInfo(bt, productInfo(at, bt)))

  override implicit def productInfo[A, B](implicit al: Loss[A], bl: Loss[B]): Loss.Aux[(A, B), (al.loss, bl.loss)] =
    new Loss[(A, B)] {
      override def convert: ((A, B)) => Eval[(A, B)] = p => pairEval(al.convert(p._1), bl.convert(p._2))

      override val lc: LossCase.Aux[(A, B), ProductLCRet[A, B]] = ProductLC[A, B]()

      override def lca: lc.ret = new ProductLCRet[A, B] {
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

  override def productFirstInfo[A, B]: Loss[(A, B)] => Loss[A] = p => witness(p.lc.unique(ProductLC[A, B]()))(p.lca).Fst

  override def productSecondInfo[A, B]: Loss[(A, B)] => Loss[B] = p => witness(p.lc.unique(ProductLC[A, B]()))(p.lca).Snd

  def curry[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]): Eval[(((A, B)) => C) => A => B => C] =
    arrowEval[((A, B)) => C, A => B => C, ArrowLoss[(A, B), ci.loss], ArrowLoss[A, ArrowLoss[B, ci.loss]]](abc =>
      (arrowEval[A, B => C, ai.loss, ArrowLoss[B, ci.loss]](a =>
        (arrowEval[B, C, bi.loss, ci.loss](b => {
          val c = aeval(abc).forward(pairEval(a, b))
          (c.eb, l => c.backward(l)._2)
        })(bi, ci), l => l.seq.map(x => {
          val c = aeval(abc).forward(pairEval(a, x._1))
          c.backward(x._2)._1
        }).foldRight(ai.m.zero)((x, y) => ai.m.append(x, y))))
        (ai, arrowInfo(bi, ci)), l => ArrowLoss(l.seq.flatMap(x => x._2.seq.map(y => (pairEval(x._1, y._1), y._2))))))

  def uncurry[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]): Eval[(A => B => C) => ((A, B)) => C] =
    arrowEval[A => B => C, ((A, B)) => C, ArrowLoss[A, ArrowLoss[B, ci.loss]], ArrowLoss[(A, B), ci.loss]](abc =>
      (arrowEval[(A, B), C, (ai.loss, bi.loss), ci.loss](ab => {
        val bc = aeval(abc).forward(peval(ab)._1)
        val c = aeval(bc.eb).forward(peval(ab)._2)
        (c.eb, l => (bc.backward(ArrowLoss(Seq((peval(ab)._2, l)))), c.backward(l)))
      })(productInfo(ai, bi), ci), l => ArrowLoss(l.seq.map(x => (peval(x._1)._1, ArrowLoss(Seq((peval(x._1)._2, x._2))))))))
}
