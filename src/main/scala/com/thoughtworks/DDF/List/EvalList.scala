package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.{EvalArrow, ArrowLoss}
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

    override val loss: Loss[List[A]] = ListInfo(ai)
  }

  override implicit def ListInfo[A](implicit ai: Loss[A]): Loss.Aux[List[A], List[ai.loss]] = new Loss[List[A]] {

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

  override def ListElmInfo[A](implicit lai: Loss[List[A]]): Loss[A] = witness(lai.lc.unique(ListLC[A]()))(lai.lca)

  override def Nil[A](implicit ai: Loss[A]): Eval[List[A]] = listEval(scala.List())

  override def Cons[A](implicit ai: Loss[A]): Eval[A => List[A] => List[A]] =
    arrowEval[A, List[A] => List[A], ai.loss, ArrowLoss[List[A], List[ai.loss]]](a =>
      (arrowEval[List[A], List[A], List[ai.loss], List[ai.loss]](la =>
        (listEval(a :: leval(la)), l => l.tail)), l =>
        l.seq.map(_._2.foldRight(ai.m.zero)((x, y) => ai.m.append(x, y))).
          foldRight(ai.m.zero)((x, y) => ai.m.append(x, y))))(ai, ArrowInfo(ListInfo(ai), ListInfo(ai)))

  override def listMatch[A, B](implicit ai: Loss[A], bi: Loss[B]): Eval[B => (A => List[A] => B) => List[A] => B] =
    arrowEval[
      B,
      (A => List[A] => B) => List[A] => B,
      bi.loss,
      ArrowLoss[A => List[A] => B, ArrowLoss[List[A], bi.loss]]](nilc =>
      (arrowEval[A => List[A] => B, List[A] => B, ArrowLoss[A, ArrowLoss[List[A], bi.loss]], ArrowLoss[List[A], bi.loss]](consc =>
        (arrowEval[List[A], B, List[ai.loss], bi.loss](li => leval(li) match {
          case scala.Nil => (nilc, _ => scala.List(ai.m.zero))
          case h :: t => {
            val lb = aeval(consc).forward(h)
            val b = aeval(lb.eb).forward(listEval(t))
            (b.eb, l => lb.backward(ArrowLoss(Seq((listEval(t), l)))) :: b.backward(l))
          }
        })(ListInfo(ai), bi), l => ArrowLoss(l.seq.filter(x => leval(x._1).nonEmpty).map(x =>
          (leval(x._1).head, ArrowLoss(Seq((listEval(leval(x._1).tail), x._2)))))))),
        l => l.seq.flatMap(x => x._2.seq.filter(y => leval(y._1).isEmpty).map(z => z._2)).
          foldRight(bi.m.zero)((x, y) => bi.m.append(x, y))))(
      bi, ArrowInfo(ArrowInfo(ai, ArrowInfo(ListInfo(ai), bi)), ArrowInfo(ListInfo(ai), bi)))

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
