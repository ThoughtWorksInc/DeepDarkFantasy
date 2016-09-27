package com.thoughtworks.DDF

import com.thoughtworks.DDF.Eval._
import com.thoughtworks.DDF.Arr._
import com.thoughtworks.DDF.Combinators._
import com.thoughtworks.DDF.Double.DEval._
import com.thoughtworks.DDF.Product.ProdEval
import com.thoughtworks.DDF.Sum.SumEval._

import scalaz.Leibniz._

class EvalLang extends Lang[Loss, Eval] with ArrEval with CombEval with ProdEval {
  override def LitD: Double => Eval[Double] = DEval

  override def PlusD: Eval[Double => Double => Double] =
    arrEval[Double, Double => Double, DLoss, ArrLoss[Double, DLoss]](l =>
      (arrEval[Double, Double, DLoss, DLoss](
        r => (DEval(deval(l) + deval(r)), rl => rl)),
        ll => DLoss(ll.seq.map(_._2.d).sum)))

  override def MultD: Eval[Double => Double => Double] =
    arrEval[Double, Double => Double, DLoss, ArrLoss[Double, DLoss]](l =>
      (arrEval[Double, Double, DLoss, DLoss](
        r => (DEval(deval(l) * deval(r)), rl => DLoss(deval(l) * rl.d))),
        ll => DLoss(ll.seq.map(l => deval(l._1) * l._2.d).sum)))

  override def DoubleInfo: Loss[Double] = dLoss

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
}