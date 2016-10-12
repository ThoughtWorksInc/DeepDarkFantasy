package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.{ArrowLoss, EvalArrow}
import com.thoughtworks.DDF.Combinators.{Comb, EvalComb}
import com.thoughtworks.DDF.{Eval, EvalCase, Loss, LossCase}

import scalaz.Monoid
import scalaz.Leibniz._

trait EvalOption extends OptionRepr[Loss, Eval] with EvalArrow {
  case class OptionLC[A]() extends LossCase[Option[A]] {
    type ret = Loss[A]
  }

  override implicit def optionInfo[A](implicit ai: Loss[A]): Loss.Aux[Option[A], ai.loss] = new Loss[Option[A]] {
    override def m: Monoid[ai.loss] = ai.m

    override def convert: Option[A] => Eval[Option[A]] = x => optionEval[A](x.map(ai.convert))

    override val lc: LossCase.Aux[Option[A], Loss[A]] = OptionLC()

    override def lca: lc.ret = ai

    override type ret = ai.loss

    override def update(x: Option[A])(rate: Double)(l: ai.loss): Option[A] = x.map(y => ai.update(y)(rate)(l))
  }

  case class EvalEC[A]() extends EvalCase[Option[A]] {
    type ret = Option[Eval[A]]
  }

  def optionEval[A](opt: Option[Eval[A]])(implicit ai: Loss[A]) = new Eval[Option[A]] {
    override val loss: Loss[Option[A]] = optionInfo(ai)

    override def eval: Option[A] = opt.map(_.eval)

    override val ec: EvalCase.Aux[Option[A], Option[Eval[A]]] = EvalEC()

    override def eca: ec.ret = opt
  }

  def oeval[A](opt: Eval[Option[A]]): Option[Eval[A]] = witness(opt.ec.unique(EvalEC[A]()))(opt.eca)

  override def optionElmInfo[A]: Loss[Option[A]] => Loss[A] = x => witness(x.lc.unique(OptionLC[A]()))(x.lca)

  override def none[A](implicit ai: Loss[A]): Eval[Option[A]] = optionEval[A](None)

  override def some[A](implicit ai: Loss[A]): Eval[A => Option[A]] =
    arrowEval[A, Option[A], ai.loss, ai.loss](a => (optionEval(Some(a)), l => l))(ai, optionInfo(ai))

  private def comb : Comb[Loss, Eval] = EvalComb.apply

  override def optionMatch[A, B](implicit ai: Loss[A], bi: Loss[B]): Eval[Option[A] => B => (A => B) => B] =
    arrowEval[Option[A], B => (A => B) => B, ai.loss, ArrowLoss[B, ArrowLoss[A => B, bi.loss]]](opt => {
      oeval(opt) match {
        case None => (comb.K[B, A => B], _ => ai.m.zero)
        case Some(a) => (app(comb.K[(A => B) => B, B](arrowInfo(arrowInfo(ai, bi), bi), bi))(app(comb.Let[A, B])(a)), l =>
          l.seq.flatMap(x => x._2.seq.map(y => aeval(y._1).forward(a).backward(y._2))).foldRight(
            ai.m.zero)((x, y) => ai.m.append(x, y)))
      }
    })
}

object EvalOption {
  implicit def apply = new EvalOption {}
}