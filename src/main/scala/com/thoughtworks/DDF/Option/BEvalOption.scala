package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.{ArrowLoss, BEvalArrow}
import com.thoughtworks.DDF.Combinators.{Comb, BEvalComb}
import com.thoughtworks.DDF.{CommutativeMonoid, BEval, BEvalCase, Loss, LossCase}

import scalaz.Leibniz._

trait BEvalOption extends Option[Loss, BEval] with BEvalArrow {
  case class OptionLC[A]() extends LossCase[scala.Option[A]] {
    type ret = Loss[A]
  }

  override implicit def optionInfo[A](implicit ai: Loss[A]): Loss.Aux[scala.Option[A], ai.loss] =
    new Loss[scala.Option[A]] {
      override def m: CommutativeMonoid[ai.loss] = ai.m

    override def convert: scala.Option[A] => BEval[scala.Option[A]] = x => optionEval[A](x.map(ai.convert))

    override val lc: LossCase.Aux[scala.Option[A], Loss[A]] = OptionLC()

    override def lca: lc.ret = ai

    override type ret = ai.loss

    override def update(x: scala.Option[A])(rate: Double)(l: ai.loss): scala.Option[A] =
      x.map(y => ai.update(y)(rate)(l))
  }

  case class OptionBEC[A]() extends BEvalCase[scala.Option[A]] {
    type ret = scala.Option[BEval[A]]
  }

  def optionEval[A](opt: scala.Option[BEval[A]])(implicit ai: Loss[A]) = new BEval[scala.Option[A]] {
    override val loss: Loss[scala.Option[A]] = optionInfo(ai)

    override def eval: scala.Option[A] = opt.map(_.eval)

    override val ec: BEvalCase.Aux[scala.Option[A], scala.Option[BEval[A]]] = OptionBEC()

    override def eca: ec.ret = opt
  }

  def oeval[A](opt: BEval[scala.Option[A]]): scala.Option[BEval[A]] = witness(opt.ec.unique(OptionBEC[A]()))(opt.eca)

  override def optionElmInfo[A]: Loss[scala.Option[A]] => Loss[A] = x => witness(x.lc.unique(OptionLC[A]()))(x.lca)

  override def none[A](implicit ai: Loss[A]): BEval[scala.Option[A]] = optionEval[A](None)

  override def some[A](implicit ai: Loss[A]): BEval[A => scala.Option[A]] =
    arrowEval[A, scala.Option[A], ai.loss, ai.loss](a => (optionEval(Some(a)), l => l))(ai, optionInfo(ai))

  private def comb : Comb[Loss, BEval] = BEvalComb.apply

  override def optionMatch[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[scala.Option[A] => B => (A => B) => B] =
    arrowEval[scala.Option[A], B => (A => B) => B, ai.loss, ArrowLoss[B, ArrowLoss[A => B, bi.loss]]](opt => {
      oeval(opt) match {
        case None => (comb.K[B, A => B], _ => ai.m.zero)
        case Some(a) => (app(comb.K[(A => B) => B, B](arrowInfo(arrowInfo(ai, bi), bi), bi))(app(comb.Let[A, B])(a)),
          _.mapReduce(b => _.mapReduce(ab => l => aeval(ab).forward(a).backward(l))(ai.m))(ai.m))
      }
    })
}

object BEvalOption {
  implicit def apply = new BEvalOption {}
}