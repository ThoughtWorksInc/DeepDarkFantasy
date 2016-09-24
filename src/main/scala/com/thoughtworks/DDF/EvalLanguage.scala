package com.thoughtworks.DDF

import Loss.{APLoss, ArrLoss, DLoss}
import com.thoughtworks.DDF.Eval._
import com.thoughtworks.DDF.Loss._

import scalaz.Leibniz._

class EvalLanguage extends Language[APLoss, ADPEval] {
  override def ArrInfo[A, B]: APLoss[A] => APLoss[B] => APLoss[A => B] = x => y => arrLoss(x, y)

  override def ArrDomInfo[A, B]: APLoss[A => B] => APLoss[A] = _.ArrDom[A, B](refl[A => B])

  override def ArrRngInfo[A, B]: APLoss[A => B] => APLoss[B] = _.ArrRng[A, B](refl[A => B])

  override def app[A, B] = f => x => f.aeval(x)(refl[A => B], x.loss, ArrRngInfo(f.loss))._1

  override def S[A, B, C](implicit at: APLoss[A], bt: APLoss[B], ct: APLoss[C]): ADPEval[(A => B => C) => (A => B) => A => C] =
    ArrEval[
      A => B => C,
      (A => B) => A => C,
      ArrLoss[A, ArrLoss[B, ct.loss]],
      ArrLoss[A => B, ArrLoss[A, ct.loss]]](
      abc => (ArrEval[A => B, A => C, ArrLoss[A, bt.loss], ArrLoss[A, ct.loss]](
        ab => (ArrEval[A, C, at.loss, ct.loss](a => {
          val bc = abc.aeval[A, B => C](a)(refl[A => B => C], at, arrLoss(bt, ct))
          val b = ab.aeval[A, B](a)(refl[A => B], at, bt)
          val c = bc._1.aeval[B, C](b._1)(refl[B => C], bt, ct)
          (c._1, l => at.m.append(bc._2(ArrLoss(Seq((b._1, l)))), b._2(c._2(l))))
        })(at, ct), l => ArrLoss(l.seq.map(x => (x._1, {
          val bc = abc.aeval[A, B => C](x._1)(refl[A => B => C], at, arrLoss(bt, ct))
          val b = ab.aeval[A, B](x._1)(refl[A => B], at, bt)
          val c = bc._1.aeval[B, C](b._1)(refl[B => C], bt, ct)
          c._2(x._2)
        }))))), l => ArrLoss(l.seq.flatMap(x => x._2.seq.map(y => (
        y._1, ArrLoss(Seq((x._1.aeval(y._1)(refl[A => B], at, bt)._1, y._2)))))))))

  override def K[A, B](implicit at: APLoss[A], bt: APLoss[B]): ADPEval[A => B => A] =
    ArrEval[A, B => A, at.loss, ArrLoss[B, at.loss]](a => (
      ArrEval[B, A, bt.loss, at.loss](_ => (a, _ => bt.m.zero))(bt, at),
      l => l.seq.map(_._2).fold(at.m.zero)((l, r) => at.m.append(l, r))))(at, arrLoss(bt, at))

  override def I[A](implicit at: APLoss[A]): ADPEval[A => A] = ArrEval[A, A, at.loss, at.loss](x => (x, y => y))(at, at)

  override def LitD: Double => ADPEval[Double] = DEval

  override def PlusD: ADPEval[Double => Double => Double] =
    ArrEval[Double, Double => Double, DLoss, ArrLoss[Double, DLoss]](l =>
      (ArrEval[Double, Double, DLoss, DLoss](
        r => (DEval(l.deval + r.deval), rl => rl)),
        ll => DLoss(ll.seq.map(_._2.x).sum)))

  override def MultD: ADPEval[Double => Double => Double] =
    ArrEval[Double, Double => Double, DLoss, ArrLoss[Double, DLoss]](l =>
      (ArrEval[Double, Double, DLoss, DLoss](
        r => (DEval(l.deval * r.deval), rl => DLoss(l.deval * rl.x))),
        ll => DLoss(ll.seq.map(l => l._1.deval * l._2.x).sum)))

  override def ReprInfo[A]: ADPEval[A] => APLoss[A] = _.loss

  override def Y[A, B](implicit at: APLoss[A], bt: APLoss[B]): ADPEval[((A => B) => A => B) => A => B] = {
    type abt = ArrLoss[A, bt.loss]
    ArrEval[(A => B) => A => B, A => B, ArrLoss[A => B, abt], abt](abab => {
      val ab = ArrEval[A, B, at.loss, bt.loss](a => app(abab)(app(Y[A, B])(abab)).aeval(a))(at, bt)
      (ab, abl => ArrLoss(abl.seq.map(p => (ab, ArrLoss(Seq(p))))))
    })
  }

  override def fst[A, B](implicit at: APLoss[A], bt: APLoss[B]): ADPEval[((A, B)) => A] =
    ArrEval[(A, B), A, (at.loss, bt.loss), at.loss](p => (p.peval(refl[(A, B)])._1, al => (al, bt.m.zero)))(pairLoss(at, bt), at)

  override def snd[A, B](implicit at: APLoss[A], bt: APLoss[B]): ADPEval[((A, B)) => B] =
    ArrEval[(A, B), B, (at.loss, bt.loss), bt.loss](p => (p.peval(refl[(A, B)])._2, bl => (at.m.zero, bl)))(pairLoss(at, bt), bt)

  override def mkPair[A, B](implicit at: APLoss[A], bt: APLoss[B]): ADPEval[(A) => (B) => (A, B)] =
    ArrEval[A, B => (A, B), at.loss, ArrLoss[B, (at.loss, bt.loss)]](a => (ArrEval[B, (A, B), bt.loss, (at.loss, bt.loss)](b =>
      (PairEval(a, b), _._2))(bt, pairLoss(at, bt)), _.seq.map(_._2._1).foldRight[at.loss](at.m.zero)((x, y) => at.m.append(x, y))))(
      at, arrLoss(bt, pairLoss(at, bt)))

  override def PairInfo[A, B]: (APLoss[A]) => (APLoss[B]) => APLoss[(A, B)] = a => b => pairLoss(a, b)

  override def PairFstInfo[A, B]: (APLoss[(A, B)]) => APLoss[A] = _.PairFst(refl[(A, B)])

  override def PairSndInfo[A, B]: (APLoss[(A, B)]) => APLoss[B] = _.PairSnd(refl[(A, B)])

  override def DoubleInfo: APLoss[Double] = dLoss
}
