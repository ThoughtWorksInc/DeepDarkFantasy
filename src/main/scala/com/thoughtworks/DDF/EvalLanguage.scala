package com.thoughtworks.DDF

import com.thoughtworks.DDF.Eval._
import com.thoughtworks.DDF.Arr.ArrEval._
import com.thoughtworks.DDF.Pair.PairEval._
import com.thoughtworks.DDF.Double.DEval._
import com.thoughtworks.DDF.Sum.SumEval._

import scalaz.Leibniz._

object EvalLanguage {

  class EvalLanguage extends Language[Loss, Eval] {
    override def ArrInfo[A, B]: Loss[A] => Loss[B] => Loss[A => B] = x => y => arrLoss(x, y)

    override def ArrDomInfo[A, B]: Loss[A => B] => Loss[A] = l => witness(l.lc.unique(ArrLC[A, B]()))(l.lca).Dom

    override def ArrRngInfo[A, B]: Loss[A => B] => Loss[B] = l => witness(l.lc.unique(ArrLC[A, B]()))(l.lca).Rng

    override def app[A, B] = f => x => aeval(f).forward(x)(ArrDomInfo(ReprInfo(f)), ArrRngInfo(ReprInfo(f))).eb

    override def S[A, B, C](implicit at: Loss[A], bt: Loss[B], ct: Loss[C]): Eval[(A => B => C) => (A => B) => A => C] =
      arrEval[
        A => B => C,
        (A => B) => A => C,
        ArrLoss[A, ArrLoss[B, ct.loss]],
        ArrLoss[A => B, ArrLoss[A, ct.loss]]](
        abc => (arrEval[A => B, A => C, ArrLoss[A, bt.loss], ArrLoss[A, ct.loss]](
          ab => (arrEval[A, C, at.loss, ct.loss](a => {
            val bc = aeval(abc).forward(a)
            val b = aeval(ab).forward(a)
            val c = aeval(bc.eb).forward(b.eb)
            (c.eb, l => at.m.append(bc.backward(ArrLoss(Seq((b.eb, l)))), b.backward(c.backward(l))))
          })(at, ct), l => ArrLoss(l.seq.map(x => (x._1, {
            val bc = aeval(abc).forward(x._1)
            val b = aeval(ab).forward(x._1)
            val c = aeval(bc.eb).forward(b.eb)
            c.backward(x._2)
          }))))), l => ArrLoss(l.seq.flatMap(x => x._2.seq.map(y => (
          y._1, ArrLoss(Seq((app(x._1)(y._1), y._2)))))))))

    override def K[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[A => B => A] =
      arrEval[A, B => A, at.loss, ArrLoss[B, at.loss]](a => (
        arrEval[B, A, bt.loss, at.loss](_ => (a, _ => bt.m.zero))(bt, at),
        l => l.seq.map(_._2).fold(at.m.zero)((l, r) => at.m.append(l, r))))(at, arrLoss(bt, at))

    override def I[A](implicit at: Loss[A]): Eval[A => A] = arrEval[A, A, at.loss, at.loss](x => (x, y => y))(at, at)

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

    override def ReprInfo[A]: Eval[A] => Loss[A] = _.loss

    override def Y[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[((A => B) => A => B) => A => B] = {
      type abt = ArrLoss[A, bt.loss]
      arrEval[(A => B) => A => B, A => B, ArrLoss[A => B, abt], abt](abab => {
        val ab = arrEval[A, B, at.loss, bt.loss](a => {
          val fa = aeval(app(abab)(app(Y[A, B])(abab))).forward(a)(at, bt)
          (fa.eb, fa.backward)
        })(at, bt)
        (ab, abl => ArrLoss(abl.seq.map(p => (ab, ArrLoss(Seq(p))))))
      })
    }

    override def fst[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[((A, B)) => A] =
      arrEval[(A, B), A, (at.loss, bt.loss), at.loss](p => (peval(p)._1, al => (al, bt.m.zero)))(pairLoss(at, bt), at)

    override def snd[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[((A, B)) => B] =
      arrEval[(A, B), B, (at.loss, bt.loss), bt.loss](p => (peval(p)._2, bl => (at.m.zero, bl)))(pairLoss(at, bt), bt)

    override def mkPair[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[(A) => (B) => (A, B)] =
      arrEval[A, B => (A, B), at.loss, ArrLoss[B, (at.loss, bt.loss)]](a => (arrEval[B, (A, B), bt.loss, (at.loss, bt.loss)](b =>
        (pairEval(a, b), _._2))(bt, pairLoss(at, bt)), _.seq.map(_._2._1).foldRight[at.loss](at.m.zero)((x, y) => at.m.append(x, y))))(
        at, arrLoss(bt, pairLoss(at, bt)))

    override def PairInfo[A, B]: Loss[A] => Loss[B] => Loss[(A, B)] = a => b => pairLoss(a, b)

    override def PairFstInfo[A, B]: Loss[(A, B)] => Loss[A] = p => witness(p.lc.unique(PairLC[A, B]()))(p.lca).Fst

    override def PairSndInfo[A, B]: Loss[(A, B)] => Loss[B] = p => witness(p.lc.unique(PairLC[A, B]()))(p.lca).Snd

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

    override def B[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]): Eval[(B => C) => (A => B) => A => C] =
      arrEval[B => C, (A => B) => A => C, ArrLoss[B, ci.loss], ArrLoss[A => B, ArrLoss[A, ci.loss]]](bc =>
        (arrEval[A => B, A => C, ArrLoss[A, bi.loss], ArrLoss[A, ci.loss]](ab => (arrEval[A, C, ai.loss, ci.loss](a => {
          val b = aeval(ab).forward(a)
          val c = aeval(bc).forward(b.eb)
          (c.eb, l => b.backward(c.backward(l)))
        })(ai, ci), l => ArrLoss(l.seq.map(x => {
          val b = aeval(ab).forward(x._1)
          val c = aeval(bc).forward(b.eb)
          (x._1, c.backward(x._2))
        })))), l => ArrLoss(l.seq.flatMap(x => x._2.seq.map(y => (app(x._1)(y._1), y._2))))))

    override def C[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]): Eval[(A => B => C) => B => A => C] =
      arrEval[A => B => C, B => A => C, ArrLoss[A, ArrLoss[B, ci.loss]], ArrLoss[B, ArrLoss[A, ci.loss]]](abc =>
        (arrEval[B, A => C, bi.loss, ArrLoss[A, ci.loss]](b =>
          (arrEval[A, C, ai.loss, ci.loss](a => {
            val bc = aeval(abc).forward(a)
            val c = aeval(bc.eb).forward(b)
            (c.eb, l => bc.backward(ArrLoss(Seq((b, l)))))
          })(ai, ci), l => l.seq.map(p => aeval(aeval(abc).forward(p._1).eb).forward(b).backward(p._2)).
            foldRight(bi.m.zero)((l, r) => bi.m.append(l, r))))(bi, arrLoss(ai, ci)), l => ArrLoss(l.seq.flatMap(b => b._2.seq.map(a =>
          (a._1, ArrLoss(Seq((b._1, a._2)))))))))

    override def W[A, B](implicit ai: Loss[A], bi: Loss[B]): Eval[(A => A => B) => A => B] =
      arrEval[A => A => B, A => B, ArrLoss[A, ArrLoss[A, bi.loss]], ArrLoss[A, bi.loss]](aab =>
        (arrEval[A, B, ai.loss, bi.loss](a => {
          val ab = aeval(aab).forward(a)
          val b = aeval(ab.eb).forward(a)
          (b.eb, bl => ai.m.append(b.backward(bl), ab.backward(ArrLoss(Seq((a, bl))))))
        })(ai, bi), l => ArrLoss(l.seq.map(x => (x._1, ArrLoss(Seq((x._1, x._2))))))))
  }

}