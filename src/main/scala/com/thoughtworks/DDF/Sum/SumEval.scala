package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arr.{ArrEval, ArrLang, ArrLoss}
import com.thoughtworks.DDF.{Eval, EvalCase, Loss, LossCase}

import scalaz.Leibniz._
import scalaz.Monoid

trait SumEval extends SumLang[Loss, Eval] with ArrEval {
  override def left[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[A => Either[A, B]] =
    arrEval[A, Either[A, B], at.loss, (at.loss, bt.loss)](ea => (sumEval(Left(ea)), _._1))(at, sumLoss(at, bt))

  override def right[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[B => Either[A, B]] =
    arrEval[B, Either[A, B], bt.loss, (at.loss, bt.loss)](eb => (sumEval(Right(eb)), _._2))(bt, sumLoss(at, bt))

  override def SumInfo[A, B]: Loss[A] => Loss[B] => Loss[Either[A, B]] = al => bl => sumLoss[A, B](al, bl)

  override def SumLeftInfo[A, B]: Loss[Either[A, B]] => Loss[A] = l => witness(l.lc.unique(SumLC[A, B]()))(l.lca).Left

  override def SumRightInfo[A, B]: Loss[Either[A, B]] => Loss[B] = l => witness(l.lc.unique(SumLC[A, B]()))(l.lca).Right

  override def sumMatch[A, B, C](implicit at: Loss[A], bt: Loss[B], ct: Loss[C]):
  Eval[(A => C) => (B => C) => Either[A, B] => C] =
    arrEval[A => C, (B => C) => Either[A, B] => C, ArrLoss[A, ct.loss], ArrLoss[B => C, ArrLoss[Either[A, B], ct.loss]]](ac =>
      (arrEval[B => C, Either[A, B] => C, ArrLoss[B, ct.loss], ArrLoss[Either[A, B], ct.loss]](bc =>
        (arrEval[Either[A, B], C, (at.loss, bt.loss), ct.loss](ab => seval(ab) match {
          case Left(a) =>
            val c = aeval(ac).forward(a)
            (c.eb, l => (c.backward(l), bt.m.zero))
          case Right(b) =>
            val c = aeval(bc).forward(b)
            (c.eb, l => (at.m.zero, c.backward(l)))
        })(sumLoss(at, bt), ct), l =>
          ArrLoss(l.seq.map(x => (seval[A, B](x._1), x._2)).
            filter(x => x._1.isRight).map(x => (x._1.right.get, x._2))))),
        l => ArrLoss(l.seq.flatMap(x =>
          x._2.seq.map(y => (seval(y._1), y._2)).filter(y => y._1.isLeft).map(y => (y._1.left.get, y._2))))))

  def sumEval[A, B](s: Either[Eval[A], Eval[B]])(implicit al: Loss[A], bl: Loss[B]) = new Eval[Either[A, B]] {
    override val loss: Loss[Either[A, B]] = sumLoss[A, B]

    override def eval: Either[A, B] = s match {
      case Left(x) => Left(x.eval)
      case Right(x) => Right(x.eval)
    }

    override val ec: EvalCase.Aux[Either[A, B], Either[Eval[A], Eval[B]]] = SumEC()

    override def eca: ec.ret = s
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
