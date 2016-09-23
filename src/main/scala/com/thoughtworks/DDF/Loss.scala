package com.thoughtworks.DDF

import scalaz.Leibniz._
import scalaz.Monoid
import Eq._
import Eval._
import scala.language.higherKinds

object Loss {

  trait Loss[Self[_], X] {
    type loss

    def m: Monoid[loss]

    def unique[Y](l: Loss[Self, Y])(implicit ev: X === Y): loss === l.loss = /*enforced by user*/ force[Nothing, Any, loss, l.loss]

    def conv : X => ADPEval[X]
  }

  trait ArrInfo[Self[_], X] {
    def ArrDom[A, B](implicit ev: X === (A => B)): Self[A]

    def ArrRng[A, B](implicit ev: X === (A => B)): Self[B]
  }

  trait NotArr[Self[_], X] extends ArrInfo[Self, X] {
    final override def ArrDom[A, B](implicit ev: ===[X, (A) => B]): Self[A] = throw new Exception("not Arrow")

    final override def ArrRng[A, B](implicit ev: ===[X, (A) => B]): Self[B] = throw new Exception("not Arrow")
  }

  trait PairInfo[Self[_], X] {
    def PairFst[A, B](implicit ev: X === (A, B)): Self[A]

    def PairSnd[A, B](implicit ev: X === (A, B)): Self[B]
  }

  trait NotPair[Self[_], X] extends PairInfo[Self, X] {
    final override def PairFst[A, B](implicit ev: ===[X, (A, B)]): Self[A] = throw new Exception("not Pair")

    final override def PairSnd[A, B](implicit ev: ===[X, (A, B)]): Self[B] = throw new Exception("not Pair")
  }

  case class DLoss(x: Double)

  implicit def dLoss = new APLoss[Double] with NotArr[APLoss, Double] with NotPair[APLoss, Double] {
    override type loss = DLoss

    override def m: Monoid[loss] = new Monoid[DLoss] {
      override def zero: DLoss = DLoss(0.0)

      override def append(f1: DLoss, f2: => DLoss): DLoss = DLoss(f1.x + f2.x)
    }

    override def conv: (Double) => ADPEval[Double] = DEval
  }

  case class ArrLoss[A, BL](seq: Seq[(ADPEval[A], BL)])

  implicit def arrLoss[A, B](implicit AL: APLoss[A], BL: APLoss[B]): APLoss.Aux[A => B, ArrLoss[A, BL.loss]] =
    new APLoss[A => B] with NotPair[APLoss, A => B] {
      override type loss = ArrLoss[A, BL.loss]

      override def m: Monoid[ArrLoss[A, BL.loss]] = new Monoid[ArrLoss[A, BL.loss]] {
        override def zero: ArrLoss[A, BL.loss] = ArrLoss(Seq())

        override def append(f1: ArrLoss[A, BL.loss], f2: => ArrLoss[A, BL.loss]): ArrLoss[A, BL.loss] =
          ArrLoss(f1.seq ++ f2.seq)
      }

      override def ArrDom[C, D](implicit ev: ===[A => B, C => D]): APLoss[C] = ArrDomEq(ev).subst[APLoss](AL)

      override def ArrRng[C, D](implicit ev: ===[A => B, C => D]): APLoss[D] = ArrRngEq(ev).subst[APLoss](BL)

      override def conv: ((A) => B) => ADPEval[(A) => B] = f => ArrEval[A, B, AL.loss, BL.loss](x => (BL.conv(f(x.eval)), ???))(AL, BL)
    }

  implicit def pairLoss[A, B](implicit al: APLoss[A], bl: APLoss[B]) = new APLoss[(A, B)] with NotArr[APLoss, (A, B)] {
    override type loss = (al.loss, bl.loss)

    override def m: Monoid[(al.loss, bl.loss)] = new Monoid[(al.loss, bl.loss)] {
      override def zero: (al.loss, bl.loss) = (al.m.zero, bl.m.zero)

      override def append(f1: (al.loss, bl.loss), f2: => (al.loss, bl.loss)): (al.loss, bl.loss) =
        (al.m.append(f1._1, f2._1), bl.m.append(f1._2, f2._2))
    }

    override def PairFst[C, D](implicit ev: ===[(A, B), (C, D)]): APLoss[C] = PairFstEq(ev).subst[APLoss](al)

    override def PairSnd[C, D](implicit ev: ===[(A, B), (C, D)]): APLoss[D] = PairSndEq(ev).subst[APLoss](bl)

    override def conv: ((A, B)) => ADPEval[(A, B)] = p => PairEval(al.conv(p._1), bl.conv(p._2))(this)
  }

  trait APLoss[X] extends Loss[APLoss, X] with ArrInfo[APLoss, X] with PairInfo[APLoss, X]

  object APLoss {
    type Aux[X, L] = APLoss[X] {type loss = L}
  }

}
