package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.{Eval, EvalCase, Loss, LossCase}

import scalaz.Leibniz._
import scalaz.Monoid
object SumEval {
  case class SumEC[A, B]() extends EvalCase[Either[A, B]] {
    override type ret = Either[Eval[A], Eval[B]]
  }

  case class sumEval[A, B](s: Either[Eval[A], Eval[B]])(implicit al: Loss[A], bl: Loss[B]) extends Eval[Either[A, B]] {
    override val loss: Loss[Either[A, B]] = sumLoss[A, B]

    override def eval: Either[A, B] = s match {
      case Left(x) => Left(x.eval)
      case Right(x) => Right(x.eval)
    }

    override val ec: EvalCase.Aux[Either[A, B], Either[Eval[A], Eval[B]]] = SumEC()

    override def eca: ec.ret = s
  }


  trait SumLCRet[A, B] {
    def Left: Loss[A]

    def Right: Loss[B]
  }

  case class SumLC[A, B]() extends LossCase[Either[A, B]] {
    override type ret = SumLCRet[A, B]
  }

  implicit def sumLoss[A, B](implicit al: Loss[A], bl: Loss[B]): Loss.Aux[Either[A, B], (al.loss, bl.loss)] =
    new Loss[Either[A, B]] {

      override def conv: Either[A, B] => Eval[Either[A, B]] = {
        case Left(x) => sumEval(Left(al.conv(x)))
        case Right(x) => sumEval(Right(bl.conv(x)))
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

  def seval[A, B](s: Eval[Either[A, B]]): Either[Eval[A], Eval[B]] = witness(s.ec.unique(SumEC[A, B]()))(s.eca)
}
