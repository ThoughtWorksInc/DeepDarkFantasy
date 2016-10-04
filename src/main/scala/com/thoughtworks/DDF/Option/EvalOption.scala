package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.{ArrowLoss, EvalArrow}
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

  override def optionMatch[A, B](implicit ai: Loss[A], bi: Loss[B]): Eval[B => (A => B) => Option[A] => B] =
    arrowEval[B, (A => B) => Option[A] => B, bi.loss, ArrowLoss[A => B, ArrowLoss[Option[A], bi.loss]]](ncase =>
      (arrowEval[A => B, Option[A] => B, ArrowLoss[A, bi.loss], ArrowLoss[Option[A], bi.loss]](scase =>
        (arrowEval[Option[A], B, ai.loss, bi.loss](oa => oeval(oa) match {
          case Some(a) => {
            val b = aeval(scase).forward(a)
            (b.eb, b.backward)
          }
          case None => (ncase, _ => ai.m.zero)
        })(optionInfo(ai), bi), l =>
          ArrowLoss(l.seq.filter(x => oeval(x._1).isDefined).map(x => (oeval(x._1).get, x._2))))), l =>
        l.seq.flatMap(x => x._2.seq.filter(x => oeval(x._1).isEmpty).map(x =>
          x._2)).foldRight(bi.m.zero)((x, y) => bi.m.append(x, y))))(
      bi, arrowInfo(arrowInfo(ai, bi), arrowInfo(optionInfo(ai), bi)))
}

object EvalOption {
  implicit def apply = new EvalOption {}
}