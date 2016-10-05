package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.{ArrowLoss, EvalArrow}
import com.thoughtworks.DDF.Combinators.EvalComb
import com.thoughtworks.DDF.{Eval, EvalCase, Loss, LossCase}

import scalaz.Leibniz._
import scalaz.Monoid

trait EvalList extends ListRepr[Loss, Eval] with EvalArrow {
  case class ListLC[A]() extends LossCase[List[A]] {
    override type ret = Loss[A]
  }

  case class ListEC[A]() extends EvalCase[List[A]] {
    override type ret = List[Eval[A]]
  }

  def leval[A](e : Eval[List[A]]): List[Eval[A]] = witness(e.ec.unique(ListEC[A]()))(e.eca)

  def listEval[A](l : List[Eval[A]])(implicit ai: Loss[A]): Eval[List[A]] = new Eval[List[A]] {
    override def eca: ec.ret = l

    override def eval: List[A] = l.map(_.eval)

    override val ec: EvalCase.Aux[List[A], List[Eval[A]]] = ListEC()

    override val loss: Loss[List[A]] = listInfo(ai)
  }

  override implicit def listInfo[A](implicit ai: Loss[A]): Loss.Aux[List[A], List[ai.loss]] = new Loss[List[A]] {

    override def convert: List[A] => Eval[List[A]] = la => listEval[A](la.map(ai.convert))

    override val lc: LossCase.Aux[List[A], Loss[A]] = ListLC()

    override def lca: lc.ret = ai

    override type ret = List[ai.loss]

    override def m: Monoid[loss] = new Monoid[loss] {
      override def zero: loss = scala.List()

      override def append(f1: loss, f2: => loss): loss =
        if(f1.length > f2.length)
          append(f2, f1) else
          f1.zip(f2).map(p => ai.m.append(p._1, p._2)) ++ f2.drop(f1.length)
    }
  }

  override def listElmInfo[A](implicit lai: Loss[List[A]]): Loss[A] = witness(lai.lc.unique(ListLC[A]()))(lai.lca)

  override def nil[A](implicit ai: Loss[A]): Eval[List[A]] = listEval(scala.List())

  override def cons[A](implicit ai: Loss[A]): Eval[A => List[A] => List[A]] =
    arrowEval[A, List[A] => List[A], ai.loss, ArrowLoss[List[A], List[ai.loss]]](a =>
      (arrowEval[List[A], List[A], List[ai.loss], List[ai.loss]](la =>
        (listEval(a :: leval(la)), l => l.tail)), l =>
        l.seq.map(_._2.foldRight(ai.m.zero)((x, y) => ai.m.append(x, y))).
          foldRight(ai.m.zero)((x, y) => ai.m.append(x, y))))(ai, arrowInfo(listInfo(ai), listInfo(ai)))

  private def comb = EvalComb.apply

  override def listMatch[A, B](implicit ai: Loss[A], bi: Loss[B]): Eval[List[A] => B => (A => List[A] => B) => B] =
    arrowEval[
      List[A],
      B => (A => List[A] => B) => B,
      List[ai.loss],
      ArrowLoss[B, ArrowLoss[A => List[A] => B, bi.loss]]](l => leval(l) match {
      case Nil => (comb.K[B, A => List[A] => B], l => List())
      case lh :: lt => (app(comb.K[(A => List[A] => B) => B, B])(app(app(comb.C[A => List[A] => B, List[A], B])(app(app(
        comb.C[A => List[A] => B, A, List[A] => B])(
        comb.I[A => List[A] => B]))(lh)))(listEval(lt))),
        l => l.seq.flatMap(x => x._2.seq.map(y => {
          val lab = aeval(y._1).forward(lh)
          val b = aeval(lab.eb).forward(listEval(lt))
          lab.backward(ArrowLoss(Seq((listEval(lt), y._2)))) :: b.backward(y._2)
        })).foldRight(listInfo(ai).m.zero)((x, y) => listInfo(ai).m.append(x, y)))
    })

  def zipEqLength[A, B] : List[A] => List[B] => List[(A, B)] = la => lb => {
    assert(la.length == lb.length)
    la.zip(lb)
  }

  override def listMap[A, B](implicit ai: Loss[A], bi: Loss[B]): Eval[(A => B) => List[A] => List[B]] =
    arrowEval[A => B, List[A] => List[B], ArrowLoss[A, bi.loss], ArrowLoss[List[A], List[bi.loss]]](ab =>
      (arrowEval[List[A], List[B], List[ai.loss], List[bi.loss]](la => {
        val lb = leval(la).map(x => aeval(ab).forward(x))
        (listEval(lb.map(_.eb)), l => zipEqLength(lb)(l).map(x => x._1.backward(x._2)))
      }), l => ArrowLoss(l.seq.flatMap(x => zipEqLength(leval(x._1))(x._2)))))
}

object EvalList {
  implicit def apply = new EvalList {}
}