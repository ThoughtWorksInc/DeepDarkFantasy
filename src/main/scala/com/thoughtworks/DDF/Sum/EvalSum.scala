package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.{EvalArrow, ArrowLoss}
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

  override def left[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[A => Either[A, B]] =
    arrowEval[A, Either[A, B], at.loss, (at.loss, bt.loss)](ea => (sumEval(scala.Left(ea)), _._1))(at, sumInfo(at, bt))

  override def right[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[B => Either[A, B]] =
    arrowEval[B, Either[A, B], bt.loss, (at.loss, bt.loss)](eb => (sumEval(scala.Right(eb)), _._2))(bt, sumInfo(at, bt))

  override implicit def sumInfo[A, B](implicit al: Loss[A], bl: Loss[B]): Loss.Aux[Either[A, B], (al.loss, bl.loss)] =
    new Loss[Either[A, B]] {

      override def convert: Either[A, B] => Eval[Either[A, B]] = {
        case Left(x) => sumEval(scala.Left(al.convert(x)))
        case Right(x) => sumEval(scala.Right(bl.convert(x)))
      }

      override val lc: LossCase.Aux[Either[A, B], SumLCRet[A, B]] = SumLC()

      override def lca: lc.ret = new SumLCRet[A, B] {
        override def Left: Loss[A] = al

        override def Right: Loss[B] = bl
      }

      override type ret = (al.loss, bl.loss)

      override def m: Monoid[(al.loss, bl.loss)] = new Monoid[(al.loss, bl.loss)] {
        override def zero: (al.loss, bl.loss) = (al.m.zero, bl.m.zero)

        override def append(f1: (al.loss, bl.loss), f2: => (al.loss, bl.loss)): (al.loss, bl.loss) =
          (al.m.append(f1._1, f2._1), bl.m.append(f1._2, f2._2))
      }
    }

  override def sumLeftInfo[A, B]: Loss[Either[A, B]] => Loss[A] = l => witness(l.lc.unique(SumLC[A, B]()))(l.lca).Left

  override def sumRightInfo[A, B]: Loss[Either[A, B]] => Loss[B] = l => witness(l.lc.unique(SumLC[A, B]()))(l.lca).Right

  override def sumMatch[A, B, C](implicit at: Loss[A], bt: Loss[B], ct: Loss[C]):
  Eval[(A => C) => (B => C) => Either[A, B] => C] =
    arrowEval[A => C, (B => C) => Either[A, B] => C, ArrowLoss[A, ct.loss], ArrowLoss[B => C, ArrowLoss[Either[A, B], ct.loss]]](ac =>
      (arrowEval[B => C, Either[A, B] => C, ArrowLoss[B, ct.loss], ArrowLoss[Either[A, B], ct.loss]](bc =>
        (arrowEval[Either[A, B], C, (at.loss, bt.loss), ct.loss](ab => seval(ab) match {
          case Left(a) =>
            val c = aeval(ac).forward(a)
            (c.eb, l => (c.backward(l), bt.m.zero))
          case Right(b) =>
            val c = aeval(bc).forward(b)
            (c.eb, l => (at.m.zero, c.backward(l)))
        })(sumInfo(at, bt), ct), l =>
          ArrowLoss(l.seq.map(x => (seval[A, B](x._1), x._2)).
            filter(x => x._1.isRight).map(x => (x._1.right.get, x._2))))),
        l => ArrowLoss(l.seq.flatMap(x =>
          x._2.seq.map(y => (seval(y._1), y._2)).filter(y => y._1.isLeft).map(y => (y._1.left.get, y._2))))))

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
