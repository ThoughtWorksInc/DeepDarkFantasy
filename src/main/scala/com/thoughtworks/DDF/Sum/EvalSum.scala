package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.{ArrowLoss, EvalArrow}
import com.thoughtworks.DDF.Combinators.{Comb, EvalComb}
import com.thoughtworks.DDF.{Eval, EvalCase, Loss, LossCase}

import scalaz.Leibniz._
import scalaz.Monoid

trait EvalSum extends SumRepr[Loss, Eval] with EvalArrow {
  trait SumLCRet[A, B] {
    def Left: Loss[A]

    def Right: Loss[B]
  }

  case class SumEC[A, B]() extends EvalCase[Either[A, B]] {
    override type ret = Either[Eval[A], Eval[B]]
  }

  case class SumLC[A, B]() extends LossCase[Either[A, B]] {
    override type ret = SumLCRet[A, B]
  }

  override def left[A, B](implicit ai: Loss[A], bi: Loss[B]): Eval[A => Either[A, B]] =
    arrowEval[A, Either[A, B], ai.loss, (ai.loss, bi.loss)](ea => (sumEval(scala.Left(ea)), _._1))(ai, sumInfo(ai, bi))

  override def right[A, B](implicit ai: Loss[A], bi: Loss[B]): Eval[B => Either[A, B]] =
    arrowEval[B, Either[A, B], bi.loss, (ai.loss, bi.loss)](eb => (sumEval(scala.Right(eb)), _._2))(bi, sumInfo(ai, bi))

  override implicit def sumInfo[A, B](implicit ai: Loss[A], bi: Loss[B]): Loss.Aux[Either[A, B], (ai.loss, bi.loss)] =
    new Loss[Either[A, B]] {

      override def convert: Either[A, B] => Eval[Either[A, B]] = {
        case Left(x) => sumEval(scala.Left(ai.convert(x)))
        case Right(x) => sumEval(scala.Right(bi.convert(x)))
      }

      override val lc: LossCase.Aux[Either[A, B], SumLCRet[A, B]] = SumLC()

      override def lca: lc.ret = new SumLCRet[A, B] {
        override def Left: Loss[A] = ai

        override def Right: Loss[B] = bi
      }

      override type ret = (ai.loss, bi.loss)

      override def m: Monoid[(ai.loss, bi.loss)] = new Monoid[(ai.loss, bi.loss)] {
        override def zero: (ai.loss, bi.loss) = (ai.m.zero, bi.m.zero)

        override def append(f1: (ai.loss, bi.loss), f2: => (ai.loss, bi.loss)): (ai.loss, bi.loss) =
          (ai.m.append(f1._1, f2._1), bi.m.append(f1._2, f2._2))
      }

      override def update(x: Either[A, B], l: (ai.loss, bi.loss), rate: Double): Either[A, B] =
        x.left.map(y => ai.update(y, l._1, rate)).right.map(y => bi.update(y, l._2, rate))
    }

  override def sumLeftInfo[A, B]: Loss[Either[A, B]] => Loss[A] = l => witness(l.lc.unique(SumLC[A, B]()))(l.lca).Left

  override def sumRightInfo[A, B]: Loss[Either[A, B]] => Loss[B] = l => witness(l.lc.unique(SumLC[A, B]()))(l.lca).Right

  private def comb: Comb[Loss, Eval] = EvalComb.apply
  override def sumMatch[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]):
  Eval[Either[A, B] => (A => C) => (B => C) => C] =
    arrowEval[
      Either[A, B],
      (A => C) => (B => C) => C,
      (ai.loss, bi.loss),
      ArrowLoss[A => C, ArrowLoss[B => C, ci.loss]]](sum => seval(sum) match {
      case Left(a) =>
        (comb.app(comb.C[B => C, A => C, C])(comb.app(comb.K[(A => C) => C, B => C])(comb.app(comb.Let[A, C])(a))), l =>
          (l.seq.flatMap(x => x._2.seq.map(y => aeval(x._1).forward(a).backward(y._2))).
            foldRight(ai.m.zero)((x, y) => ai.m.append(x, y)), bi.m.zero))
      case Right(b) =>
        (comb.app(comb.K[(B => C) => C, A => C])(comb.app(comb.Let[B, C])(b)), l =>
          (ai.m.zero, l.seq.flatMap(x => x._2.seq.map(y => aeval(y._1).forward(b).backward(y._2))).
            foldRight(bi.m.zero)((x, y) => bi.m.append(x, y))))
    })

  def sumEval[A, B](s: Either[Eval[A], Eval[B]])(implicit al: Loss[A], bl: Loss[B]) = new Eval[Either[A, B]] {
    override val loss: Loss[Either[A, B]] = sumInfo[A, B]

    override def eval: Either[A, B] = s match {
      case Left(x) => scala.Left(x.eval)
      case Right(x) => scala.Right(x.eval)
    }

    override val ec: EvalCase.Aux[Either[A, B], Either[Eval[A], Eval[B]]] = SumEC()

    override def eca: ec.ret = s
  }

  def seval[A, B](s: Eval[Either[A, B]]): Either[Eval[A], Eval[B]] = witness(s.ec.unique(SumEC[A, B]()))(s.eca)
}
