package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.{ArrowLoss, BEvalArrow}
import com.thoughtworks.DDF.Combinators.{Comb, BEvalComb}
import com.thoughtworks.DDF.{CommutativeMonoid, BEval, BEvalCase, Loss, LossCase}

import scalaz.Leibniz._

trait BEvalSum extends SumRepr[Loss, BEval] with BEvalArrow {
  trait SumLCRet[A, B] {
    def Left: Loss[A]

    def Right: Loss[B]
  }

  case class SumBEC[A, B]() extends BEvalCase[Either[A, B]] {
    override type ret = Either[BEval[A], BEval[B]]
  }

  case class SumLC[A, B]() extends LossCase[Either[A, B]] {
    override type ret = SumLCRet[A, B]
  }

  override def left[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[A => Either[A, B]] =
    arrowEval[A, Either[A, B], ai.loss, (ai.loss, bi.loss)](ea => (sumEval(scala.Left(ea)), _._1))(ai, sumInfo(ai, bi))

  override def right[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[B => Either[A, B]] =
    arrowEval[B, Either[A, B], bi.loss, (ai.loss, bi.loss)](eb => (sumEval(scala.Right(eb)), _._2))(bi, sumInfo(ai, bi))

  override implicit def sumInfo[A, B](implicit ai: Loss[A], bi: Loss[B]): Loss.Aux[Either[A, B], (ai.loss, bi.loss)] =
    new Loss[Either[A, B]] {

      override def convert: Either[A, B] => BEval[Either[A, B]] = {
        case Left(x) => sumEval(scala.Left(ai.convert(x)))
        case Right(x) => sumEval(scala.Right(bi.convert(x)))
      }

      override val lc: LossCase.Aux[Either[A, B], SumLCRet[A, B]] = SumLC()

      override def lca: lc.ret = new SumLCRet[A, B] {
        override def Left: Loss[A] = ai

        override def Right: Loss[B] = bi
      }

      override type ret = (ai.loss, bi.loss)

      override def m: CommutativeMonoid[(ai.loss, bi.loss)] = new CommutativeMonoid[(ai.loss, bi.loss)] {
        override def zero: (ai.loss, bi.loss) = (ai.m.zero, bi.m.zero)

        override def append(f1: (ai.loss, bi.loss), f2: => (ai.loss, bi.loss)): (ai.loss, bi.loss) =
          (ai.m.append(f1._1, f2._1), bi.m.append(f1._2, f2._2))
      }

      override def update(x: Either[A, B])(rate: Double)(l: (ai.loss, bi.loss)): Either[A, B] =
        x.left.map(y => ai.update(y)(rate)(l._1)).right.map(y => bi.update(y)(rate)(l._2))
    }

  override def sumLeftInfo[A, B]: Loss[Either[A, B]] => Loss[A] = l => witness(l.lc.unique(SumLC[A, B]()))(l.lca).Left

  override def sumRightInfo[A, B]: Loss[Either[A, B]] => Loss[B] = l => witness(l.lc.unique(SumLC[A, B]()))(l.lca).Right

  private val comb: Comb[Loss, BEval] = BEvalComb.apply

  override def sumMatch[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]):
  BEval[Either[A, B] => (A => C) => (B => C) => C] =
    arrowEval[
      Either[A, B],
      (A => C) => (B => C) => C,
      (ai.loss, bi.loss),
      ArrowLoss[A => C, ArrowLoss[B => C, ci.loss]]](sum => seval(sum) match {
      case Left(a) =>
        (comb.app(comb.C[B => C, A => C, C])(comb.app(comb.K[(A => C) => C, B => C])(comb.app(comb.Let[A, C])(a))),
          _.mapReduce(ac => _.mapReduce(bc => l => (aeval(ac).forward(a).backward(l), bi.m.zero))(
            sumInfo(ai, bi).m))(sumInfo(ai, bi).m))
      case Right(b) =>
        (comb.app(comb.K[(B => C) => C, A => C])(comb.app(comb.Let[B, C])(b)),
          _.mapReduce(ac => _.mapReduce(bc => l => (ai.m.zero, aeval(bc).forward(b).backward(l)))(
          sumInfo(ai, bi).m))(sumInfo(ai, bi).m))
    })

  def sumEval[A, B](s: Either[BEval[A], BEval[B]])(implicit al: Loss[A], bl: Loss[B]) = new BEval[Either[A, B]] {
    override val loss: Loss[Either[A, B]] = sumInfo[A, B]

    override def eval: Either[A, B] = s match {
      case Left(x) => scala.Left(x.eval)
      case Right(x) => scala.Right(x.eval)
    }

    override val ec: BEvalCase.Aux[Either[A, B], Either[BEval[A], BEval[B]]] = SumBEC()

    override def eca: ec.ret = s
  }

  def seval[A, B](s: BEval[Either[A, B]]): Either[BEval[A], BEval[B]] = witness(s.ec.unique(SumBEC[A, B]()))(s.eca)

  override def sumComm[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[Either[A, B] => Either[B, A]] =
    arrowEval[Either[A, B], Either[B, A], (ai.loss, bi.loss), (bi.loss, ai.loss)](s =>
      (sumEval(seval(s).swap), l => (l._2, l._1)))

  override def sumAssocLR[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]):
  BEval[Either[Either[A, B], C] => Either[A, Either[B, C]]] =
    arrowEval[
      Either[Either[A, B], C],
      Either[A, Either[B, C]],
      ((ai.loss, bi.loss), ci.loss),
      (ai.loss, (bi.loss, ci.loss))](s =>
      (seval(s) match {
        case Left(l) => seval(l) match {
          case Left(x) => sumEval(Left(x))
          case Right(y) => sumEval(Right(sumEval(Left(y))))
        }
        case Right(r) => sumEval(Right(sumEval(Right(r))))
      },
        l => ((l._1, l._2._1), l._2._2)))

  override def sumAssocRL[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]):
  BEval[Either[A, Either[B, C]] => Either[Either[A, B], C]] =
    arrowEval[
      Either[A, Either[B, C]],
      Either[Either[A, B], C],
      (ai.loss, (bi.loss, ci.loss)),
      ((ai.loss, bi.loss), ci.loss)](s =>
      (seval(s) match {
        case Left(l) => sumEval(Left(sumEval(Left(l))))
        case Right(r) => seval(r) match {
          case Left(x) => sumEval(Left(sumEval(Right(x))))
          case Right(y) => sumEval(Right(y))
        }
      }, l => (l._1._1, (l._1._2, l._2))))
}

object BEvalSum {
  implicit def apply: BEvalSum = new BEvalSum {}
}