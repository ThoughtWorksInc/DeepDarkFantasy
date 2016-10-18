package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.{ArrowLoss, BEvalArrow}
import com.thoughtworks.DDF.Combinators.{Comb, BEvalComb}
import com.thoughtworks.DDF.{CommutativeMonoid, BEval, BEvalCase, Loss, LossCase}

import scalaz.Leibniz._

trait BEvalOption extends OptionRepr[Loss, BEval] with BEvalArrow {
  case class OptionLC[A]() extends LossCase[Option[A]] {
    type ret = Loss[A]
  }

  override implicit def optionInfo[A](implicit ai: Loss[A]): Loss.Aux[Option[A], ai.loss] = new Loss[Option[A]] {
    override def m: CommutativeMonoid[ai.loss] = ai.m

    override def convert: Option[A] => BEval[Option[A]] = x => optionEval[A](x.map(ai.convert))

    override val lc: LossCase.Aux[Option[A], Loss[A]] = OptionLC()

    override def lca: lc.ret = ai

    override type ret = ai.loss

    override def update(x: Option[A])(rate: Double)(l: ai.loss): Option[A] = x.map(y => ai.update(y)(rate)(l))
  }

  case class OptionBEC[A]() extends BEvalCase[Option[A]] {
    type ret = Option[BEval[A]]
  }

  def optionEval[A](opt: Option[BEval[A]])(implicit ai: Loss[A]) = new BEval[Option[A]] {
    override val loss: Loss[Option[A]] = optionInfo(ai)

    override def eval: Option[A] = opt.map(_.eval)

    override val ec: BEvalCase.Aux[Option[A], Option[BEval[A]]] = OptionBEC()

    override def eca: ec.ret = opt
  }

  def oeval[A](opt: BEval[Option[A]]): Option[BEval[A]] = witness(opt.ec.unique(OptionBEC[A]()))(opt.eca)

  override def optionElmInfo[A]: Loss[Option[A]] => Loss[A] = x => witness(x.lc.unique(OptionLC[A]()))(x.lca)

  override def none[A](implicit ai: Loss[A]): BEval[Option[A]] = optionEval[A](None)

  override def some[A](implicit ai: Loss[A]): BEval[A => Option[A]] =
    arrowEval[A, Option[A], ai.loss, ai.loss](a => (optionEval(Some(a)), l => l))(ai, optionInfo(ai))

  private def comb : Comb[Loss, BEval] = BEvalComb.apply

  override def optionMatch[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[Option[A] => B => (A => B) => B] =
    arrowEval[Option[A], B => (A => B) => B, ai.loss, ArrowLoss[B, ArrowLoss[A => B, bi.loss]]](opt => {
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