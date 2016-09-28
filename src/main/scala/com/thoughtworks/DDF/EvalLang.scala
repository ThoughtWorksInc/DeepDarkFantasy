package com.thoughtworks.DDF

import com.thoughtworks.DDF.Arr.ArrLoss
import com.thoughtworks.DDF.Combinators.CombEval
import com.thoughtworks.DDF.Double.DEval
import com.thoughtworks.DDF.List.ListEval
import com.thoughtworks.DDF.Product.ProdEval
import com.thoughtworks.DDF.Sum.SumEval

import scalaz.Monoid
import scalaz.Leibniz._

class EvalLang extends Lang[Loss, Eval] with ProdEval with CombEval with DEval with SumEval with ListEval {
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

    override def conv: List[A] => Eval[List[A]] = la => listEval[A](la.map(ai.conv))

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

  override implicit def UnitInfo: Loss.Aux[Unit, Unit] = new Loss[Unit] {
    override type ret = Unit

    override def m: Monoid[loss] = new Monoid[loss] {
      override def zero: loss = ()

      override def append(f1: loss, f2: => loss): loss = ()
    }

    override def conv: Unit => Eval[Unit] = _ => mkUnit

    override val lc: LossCase.Aux[Unit, Unit] = new LossCase[Unit] {
      override type ret = Unit
    }

    override def lca: lc.ret = ()
  }

  override def mkUnit: Eval[Unit] = new Eval[Unit] {
    override def eca: ec.ret = ()

    override def eval: Unit = ()

    override val ec: EvalCase.Aux[Unit, Unit] = new EvalCase[Unit] {
      override type ret = Unit
    }

    override val loss: Loss[Unit] = UnitInfo
  }

  override def Cons[A](implicit ai: Loss[A]): Eval[A => List[A] => List[A]] =
    arrEval[A, List[A] => List[A], ai.loss, ArrLoss[List[A], List[ai.loss]]](a =>
      (arrEval[List[A], List[A], List[ai.loss], List[ai.loss]](la =>
        (listEval(a :: leval(la)), l => l.tail)), l =>
        l.seq.map(_._2.foldRight(ai.m.zero)((x, y) => ai.m.append(x, y))).
          foldRight(ai.m.zero)((x, y) => ai.m.append(x, y))))(ai, ArrInfo(ListInfo(ai), ListInfo(ai)))

  override def listMatch[A, B](implicit ai: Loss[A], bi: Loss[B]): Eval[(Unit => B) => (A => List[A] => B) => List[A] => B] =
    arrEval[
      Unit => B,
      (A => List[A] => B) => List[A] => B,
      ArrLoss[Unit, bi.loss],
      ArrLoss[A => List[A] => B, ArrLoss[List[A], bi.loss]]](nilc =>
      (arrEval[A => List[A] => B, List[A] => B, ArrLoss[A, ArrLoss[List[A], bi.loss]], ArrLoss[List[A], bi.loss]](consc =>
        (arrEval[List[A], B, List[ai.loss], bi.loss](li => leval(li) match {
          case scala.Nil => (app(nilc)(mkUnit), _ => scala.List(ai.m.zero))
          case h :: t => {
            val lb = aeval(consc).forward(h)
            val b = aeval(lb.eb).forward(listEval(t))
            (b.eb, l => lb.backward(ArrLoss(Seq((listEval(t), l)))) :: b.backward(l))
          }
        })(ListInfo(ai), bi), l => ArrLoss(l.seq.filter(x => leval(x._1).nonEmpty).map(x =>
          (leval(x._1).head, ArrLoss(Seq((listEval(leval(x._1).tail), x._2)))))))),
        l => ArrLoss(l.seq.flatMap(x => x._2.seq.filter(y => leval(y._1).isEmpty).map(z => (mkUnit, z._2))))))
}