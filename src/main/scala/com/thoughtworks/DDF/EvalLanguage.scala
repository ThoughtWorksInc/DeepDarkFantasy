package com.thoughtworks.DDF

import scalaz.Leibniz._
import scalaz.Monoid

object EvalLanguage {

  trait EvalCase[X] extends TypeCase[EvalCase, X]

  object EvalCase {
    type Aux[X, Y] = EvalCase[X] {type ret = Y}
  }

  trait ArrEval[A, B] {

    trait backward[AL, BL] {
      def eb: Eval[B]

      def backward: BL => AL
    }

    def forward(ea: Eval[A])(implicit al: Loss[A], bl: Loss[B]): backward[al.loss, bl.loss]
  }

  case class ArrEC[A, B]() extends EvalCase[A => B] {
    override type ret = ArrEval[A, B]
  }

  case class PairEC[A, B]() extends EvalCase[(A, B)] {
    override type ret = (Eval[A], Eval[B])
  }

  case class SumEC[A, B]() extends EvalCase[Either[A, B]] {
    override type ret = Either[Eval[A], Eval[B]]
  }

  def arrEval[A, B, AL, BL](f: Eval[A] => (Eval[B], BL => AL))(implicit al: Loss.Aux[A, AL], bl: Loss.Aux[B, BL]) =
    new Eval[A => B] {
      override val loss: Loss[A => B] = arrLoss(al, bl)

      override def eval: A => B = a => eca.forward(al.conv(a)).eb.eval

      override val ec: EvalCase.Aux[A => B, ArrEval[A, B]] = ArrEC[A, B]()

      override def eca: ec.ret = new ArrEval[A, B] {
        override def forward(ea: Eval[A])(implicit al1: Loss[A], bl1: Loss[B]): backward[al1.loss, bl1.loss] =
          new backward[al1.loss, bl1.loss] {
            lazy val fea = f(ea)

            override lazy val eb: Eval[B] = fea._1

            override def backward: bl1.loss => al1.loss = bl2 => witness(al.unique(al1))(fea._2(witness(bl1.unique(bl))(bl2)))
          }
      }
    }

  def pairEval[A, B](a: Eval[A], b: Eval[B])(implicit al: Loss[A], bl: Loss[B]) = new Eval[(A, B)] {
    override val loss: Loss[(A, B)] = pairLoss(al, bl)

    override def eval: (A, B) = (a.eval, b.eval)

    override val ec: EvalCase.Aux[(A, B), (Eval[A], Eval[B])] = PairEC()

    override def eca: ec.ret = (a, b)
  }

  object DEC extends EvalCase[Double] {
    override type ret = Double
  }

  case class DEval(d: Double) extends Eval[Double] {
    override val loss: Loss[Double] = dLoss

    override def eval: Double = d

    override val ec: EvalCase.Aux[Double, Double] = DEC

    override def eca: ec.ret = d
  }

  case class sumEval[A, B](s : Either[Eval[A], Eval[B]])(implicit al: Loss[A], bl: Loss[B]) extends Eval[Either[A, B]] {
    override val loss: Loss[Either[A, B]] = sumLoss[A, B]

    override def eval: Either[A, B] = s match {
      case Left(x) => Left(x.eval)
      case Right(x) => Right(x.eval)
    }

    override val ec: EvalCase.Aux[Either[A, B], Either[Eval[A], Eval[B]]] = SumEC()

    override def eca: ec.ret = s
  }

  trait Eval[X] {
    val loss: Loss[X]

    def eval: /*should not be used when defining instance of Eval*/ X

    val ec: EvalCase[X]

    def eca: ec.ret
  }

  trait LossCase[X] extends TypeCase[LossCase, X]

  object LossCase {
    type Aux[X, Y] = LossCase[X] {type ret = Y}
  }

  object DoubleLC extends LossCase[Double] {

    trait ret

  }

  trait ArrLCRet[A, B] {
    def Dom: Loss[A]

    def Rng: Loss[B]
  }

  case class ArrLC[A, B]() extends LossCase[A => B] {
    override type ret = ArrLCRet[A, B]
  }

  trait PairLCRet[A, B] {
    def Fst: Loss[A]

    def Snd: Loss[B]
  }

  case class PairLC[A, B]() extends LossCase[(A, B)] {
    override type ret = PairLCRet[A, B]
  }

  trait SumLCRet[A, B] {
    def Left: Loss[A]

    def Right: Loss[B]
  }

  case class SumLC[A, B]() extends LossCase[Either[A, B]] {
    override type ret = SumLCRet[A, B]
  }

  implicit def pairLoss[A, B](implicit al: Loss[A], bl: Loss[B]): Loss.Aux[(A, B), (al.loss, bl.loss)] = new Loss[(A, B)] {

    override def conv: ((A, B)) => Eval[(A, B)] = p => pairEval(al.conv(p._1), bl.conv(p._2))

    override val lc: LossCase.Aux[(A, B), PairLCRet[A, B]] = PairLC[A, B]()

    override def lca: lc.ret = new PairLCRet[A, B] {
      override def Fst: Loss[A] = al

      override def Snd: Loss[B] = bl
    }

    override type ret = (al.loss, bl.loss)

    override def m: Monoid[(al.loss, bl.loss)] = new Monoid[(al.loss, bl.loss)] {
      override def zero: (al.loss, bl.loss) = (al.m.zero, bl.m.zero)

      override def append(f1: (al.loss, bl.loss), f2: => (al.loss, bl.loss)): (al.loss, bl.loss) =
        (al.m.append(f1._1, f2._1), bl.m.append(f1._2, f2._2))
    }
  }

  case class ArrLoss[A, BL](seq: Seq[(Eval[A], BL)])

  implicit def arrLoss[A, B](implicit al: Loss[A], bl: Loss[B]): Loss.Aux[A => B, ArrLoss[A, bl.loss]] = new Loss[A => B] {

    override type ret = ArrLoss[A, bl.loss]

    override def m: Monoid[loss] = new Monoid[loss] {
      override def zero: loss = ArrLoss(Seq())

      override def append(f1: loss, f2: => loss): loss = ArrLoss(f1.seq ++ f2.seq)
    }

    override def conv: (A => B) => Eval[A => B] = ab => arrEval[A, B, al.loss, bl.loss](a =>
      (bl.conv(ab(a.eval)), _ => al.m.zero))(al, bl)

    override val lc: LossCase.Aux[A => B, ArrLCRet[A, B]] = ArrLC()

    override def lca: lc.ret = new ArrLCRet[A, B] {
      override def Dom: Loss[A] = al

      override def Rng: Loss[B] = bl
    }
  }

  case class DLoss(d: Double)

  implicit def dLoss: Loss.Aux[Double, DLoss] = new Loss[Double] {
    override def m: Monoid[DLoss] = new Monoid[DLoss] {
      override def zero: DLoss = DLoss(0)

      override def append(f1: DLoss, f2: => DLoss): DLoss = DLoss(f1.d + f2.d)
    }

    override def conv: Double => Eval[Double] = DEval

    override val lc: LossCase.Aux[Double, DoubleLC.ret] = DoubleLC

    override def lca: lc.ret = new DoubleLC.ret {}

    override type ret = DLoss
  }

  implicit def sumLoss[A, B](implicit al : Loss[A], bl : Loss[B]) : Loss.Aux[Either[A, B], (al.loss, bl.loss)] =
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

  def aeval[A, B](ab: Eval[A => B]): ArrEval[A, B] = witness(ab.ec.unique(ArrEC[A, B]()))(ab.eca)

  def peval[A, B](ab: Eval[(A, B)]): (Eval[A], Eval[B]) = witness(ab.ec.unique(PairEC[A, B]()))(ab.eca)

  def deval(d: Eval[Double]): Double = witness(d.ec.unique(DEC))(d.eca)

  def seval[A, B](s : Eval[Either[A, B]]): Either[Eval[A], Eval[B]] = witness(s.ec.unique(SumEC[A, B]()))(s.eca)

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
  }

}