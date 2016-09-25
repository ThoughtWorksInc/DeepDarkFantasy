package com.thoughtworks.DDF

import scalaz.Leibniz._
import scalaz.Monoid

object EvalLanguage {

  trait EvalCase[X] extends TypeCase[EvalCase, X]

  trait ArrEval[A, B] {
    def forward(ea: Eval[A]): {
      val eb: Eval[B]
      def backward: eb.loss.loss => ea.loss.loss
    }
  }

  case class ArrEC[A, B]() extends EvalCase[A => B] {
    override type ret = ArrEval[A, B]
  }

  def arrEval[A, B, AL, BL](f: Eval[A] => (Eval[B], BL => AL))(implicit al: Loss.Aux[A, AL], bl: Loss.Aux[B, BL]) =
    new Eval[A => B] {
      override val loss: Loss[A => B] = arrLoss(al, bl)

      override def eval: A => B = a => eca.forward(al.conv(a)).eb.eval

      override val ec: EvalCase[A => B] {type ret = ArrEval[A, B]} = ArrEC[A, B]()

      override def eca: ec.ret = new ArrEval[A, B] {
        override def forward(ea: Eval[A]): {def backward: (eb.loss.loss) => ea.loss.loss; val eb: Eval[B]} = new {
          val fea = f(ea)
          override def backward: eb.loss.loss => ea.loss.loss = ebl =>
            witness(al.unique(ea.loss))(fea._2(witness(eb.loss.unique(bl))(ebl)))
          override val eb: Eval[B] = fea._1
        }
      }
    }

  trait Eval[X] {
    val loss: Loss[X]

    def eval: X

    val ec: EvalCase[X]

    def eca: ec.ret
  }

  trait LossCase[X] extends TypeCase[LossCase, X]

  object DoubleLC extends LossCase[Double]

  case class ArrLC[A, B]() extends LossCase[A => B] {

    trait ret {
      def Dom: Loss[A]

      def Rng: Loss[B]
    }

  }

  case class PairLC[A, B]() extends LossCase[(A, B)]

  case class ArrLoss[A, BL](seq: Seq[(A, BL)])

  implicit def arrLoss[A, B](a: Loss[A], b: Loss[B]) = new Loss[A => B] {

    override def m: Monoid[loss] = ???

    override def conv: ((A) => B) => Eval[(A) => B] = ???

    override val lc: LossCase[(A) => B] = ???

    override def lca: lc.ret = ???
  }

  trait Loss[X] extends TypeCase[Loss, X] {
    final type loss = ret

    def m: Monoid[loss]

    def conv: X => Eval[X]

    val lc: LossCase[X]

    def lca: lc.ret
  }

  object Loss {
    type Aux[X, XL] = Loss[X] {type ret = XL}
  }

  def aeval[A, B](ab : Eval[A => B]) : ArrEval[A, B] = witness(ab.ec.unique(ArrEC[A, B]()))(ab.eca)
  class EvalLanguage extends Language[Loss, Eval] {
    override def ArrInfo[A, B]: Loss[A] => Loss[B] => Loss[A => B] = x => y => arrLoss(x, y)

    override def ArrDomInfo[A, B]: Loss[A => B] => Loss[A] = l => witness(l.lc.unique(ArrLC[A, B]()))(l.lca).Dom

    override def ArrRngInfo[A, B]: Loss[A => B] => Loss[B] = l => witness(l.lc.unique(ArrLC[A, B]()))(l.lca).Rng

    override def app[A, B] = f => x => aeval(f).forward(x).eb

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
            val bc = abc.aeval[A, B => C](x._1)(refl[A => B => C], at, arrLoss(bt, ct))
            val b = ab.aeval[A, B](x._1)(refl[A => B], at, bt)
            val c = bc._1.aeval[B, C](b._1)(refl[B => C], bt, ct)
            c._2(x._2)
          }))))), l => ArrLoss(l.seq.flatMap(x => x._2.seq.map(y => (
          y._1, ArrLoss(Seq((x._1.aeval(y._1)(refl[A => B], at, bt)._1, y._2)))))))))

    override def K[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[A => B => A] =
      ArrEval[A, B => A, at.loss, ArrLoss[B, at.loss]](a => (
        ArrEval[B, A, bt.loss, at.loss](_ => (a, _ => bt.m.zero))(bt, at),
        l => l.seq.map(_._2).fold(at.m.zero)((l, r) => at.m.append(l, r))))(at, arrLoss(bt, at))

    override def I[A](implicit at: Loss[A]): Eval[A => A] = ArrEval[A, A, at.loss, at.loss](x => (x, y => y))(at, at)

    override def LitD: Double => Eval[Double] = DEval

    override def PlusD: Eval[Double => Double => Double] =
      ArrEval[Double, Double => Double, DLoss, ArrLoss[Double, DLoss]](l =>
        (ArrEval[Double, Double, DLoss, DLoss](
          r => (DEval(l.deval + r.deval), rl => rl)),
          ll => DLoss(ll.seq.map(_._2.x).sum)))

    override def MultD: Eval[Double => Double => Double] =
      ArrEval[Double, Double => Double, DLoss, ArrLoss[Double, DLoss]](l =>
        (ArrEval[Double, Double, DLoss, DLoss](
          r => (DEval(l.deval * r.deval), rl => DLoss(l.deval * rl.x))),
          ll => DLoss(ll.seq.map(l => l._1.deval * l._2.x).sum)))

    override def ReprInfo[A]: Eval[A] => Loss[A] = _.loss

    override def Y[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[((A => B) => A => B) => A => B] = {
      type abt = ArrLoss[A, bt.loss]
      ArrEval[(A => B) => A => B, A => B, ArrLoss[A => B, abt], abt](abab => {
        val ab = ArrEval[A, B, at.loss, bt.loss](a => app(abab)(app(Y[A, B])(abab)).aeval(a))(at, bt)
        (ab, abl => ArrLoss(abl.seq.map(p => (ab, ArrLoss(Seq(p))))))
      })
    }

    override def fst[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[((A, B)) => A] =
      ArrEval[(A, B), A, (at.loss, bt.loss), at.loss](p => (p.peval(refl[(A, B)])._1, al => (al, bt.m.zero)))(pairLoss(at, bt), at)

    override def snd[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[((A, B)) => B] =
      ArrEval[(A, B), B, (at.loss, bt.loss), bt.loss](p => (p.peval(refl[(A, B)])._2, bl => (at.m.zero, bl)))(pairLoss(at, bt), bt)

    override def mkPair[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[(A) => (B) => (A, B)] =
      ArrEval[A, B => (A, B), at.loss, ArrLoss[B, (at.loss, bt.loss)]](a => (ArrEval[B, (A, B), bt.loss, (at.loss, bt.loss)](b =>
        (PairEval(a, b), _._2))(bt, pairLoss(at, bt)), _.seq.map(_._2._1).foldRight[at.loss](at.m.zero)((x, y) => at.m.append(x, y))))(
        at, arrLoss(bt, pairLoss(at, bt)))

    override def PairInfo[A, B]: Loss[A] => Loss[B] => Loss[(A, B)] = a => b => pairLoss(a, b)

    override def PairFstInfo[A, B]: Loss[(A, B)] => Loss[A] = _.PairFst(refl[(A, B)])

    override def PairSndInfo[A, B]: Loss[(A, B)] => Loss[B] = _.PairSnd(refl[(A, B)])

    override def DoubleInfo: Loss[Double] = dLoss
  }

}