package com.thoughtworks.DDF

import Loss.APLoss
import com.thoughtworks.DDF.Loss._
import com.thoughtworks.DDF.Eq._

import scalaz.Leibniz._

object Eval {
  trait Eval[X] {
    def aeval[A, B](a: Eval[A])(implicit ev: X === (A => B), AL: APLoss[A], BL: APLoss[B]): (Eval[B], BL.loss => AL.loss)

    def deval(implicit ev: X === Double): Double

    def peval[A, B](implicit ev: X === (A, B)): (Eval[A], Eval[B])

    def loss: APLoss[X]
  }

  def PairEval[A, B](a: Eval[A], b: Eval[B])(implicit l: APLoss[(A, B)]) = new Eval[(A, B)] {
    override def aeval[C, D](a: Eval[C])(
      implicit ev: ===[(A, B), C => D], CL: APLoss[C], DL: APLoss[D]): (Eval[D], DL.loss => CL.loss) = throw new Exception("not ArrEval")

    override def deval(implicit ev: ===[(A, B), Double]): Double = throw new Exception("not DEval")

    override def peval[C, D](implicit ev: ===[(A, B), (C, D)]): (Eval[C], Eval[D]) =
      (PairFstEq(ev).subst[Eval](a), PairSndEq(ev).subst[Eval](b))

    override def loss: APLoss[(A, B)] = l
  }

  def ArrEval[A, B, AL, BL](forward: Eval[A] => (Eval[B], BL => AL))(implicit al: APLoss.Aux[A, AL], bl: APLoss.Aux[B, BL]) =
    new Eval[A => B] {
      override def aeval[C, D](c: Eval[C])(
        implicit ev: (A => B) === (C => D), CL: APLoss[C], DL: APLoss[D]): (Eval[D], DL.loss => CL.loss) = {
        val ac = ArrDomEq(ev)
        val bd = ArrRngEq(ev)
        val f = forward(symm[Nothing, Any, A, C](ac).subst[Eval](c))
        (bd.subst[Eval](f._1), dl =>
          witness(symm[Nothing, Any, CL.loss, AL](CL.unique(al)(symm[Nothing, Any, A, C](ac))))(
            f._2(witness(DL.unique(bl)(symm[Nothing, Any, B, D](bd)))(dl))))
      }

      override def deval(implicit ev: ===[A => B, Double]): Double = throw new Exception("not DEval")

      override def loss: APLoss[A => B] = arrLoss(al, bl)

      override def peval[C, D](implicit ev: ===[(A) => B, (C, D)]): (Eval[C], Eval[D]) = throw new Exception("not PEval")
    }

  case class DEval(d: Double) extends Eval[Double] {
    val eval = d

    override def aeval[A, B](a: Eval[A])(implicit ev: ===[Double, A => B], AL: APLoss[A], BL: APLoss[B]):
    (Eval[B], BL.loss => AL.loss) = throw new Exception("not ArrEval")

    override def peval[A, B](implicit ev: ===[Double, (A, B)]): (Eval[A], Eval[B]) = throw new Exception("not PEval")

    override def deval(implicit ev: ===[Double, Double]): Double = d

    override def loss: APLoss[Double] = dLoss
  }
}
