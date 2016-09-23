package com.thoughtworks.DDF

import Loss.APLoss
import com.thoughtworks.DDF.Loss._
import com.thoughtworks.DDF.Eq._

import scala.language.higherKinds
import scalaz.Leibniz._

object Eval {

  trait Eval[Repr[_], X] {
    def loss: APLoss[X]

    def eval: X
  }

  trait WAEval[Repr[_], X] {
    def aeval[A, B](a: Repr[A])(implicit ev: X === (A => B), AL: APLoss[A], BL: APLoss[B]): (Repr[B], BL.loss => AL.loss)
  }

  trait WDEval[Repr[_], X] {
    def deval(implicit ev: X === Double): Double
  }

  trait WPEval[Repr[_], X] {
    def peval[A, B](implicit ev: X === (A, B)): (ADPEval[A], ADPEval[B])
  }

  trait ADPEval[X] extends Eval[ADPEval, X] with WAEval[ADPEval, X] with WDEval[ADPEval, X] with WPEval[ADPEval, X]

  def PairEval[A, B](a: ADPEval[A], b: ADPEval[B])(implicit l: APLoss[(A, B)]) = new ADPEval[(A, B)] {
    override def aeval[C, D](a: ADPEval[C])(implicit ev: ===[(A, B), C => D], CL: APLoss[C], DL: APLoss[D]):
    (ADPEval[D], DL.loss => CL.loss) = throw new Exception("not ArrEval")

    override def deval(implicit ev: ===[(A, B), Double]): Double = throw new Exception("not DEval")

    override def peval[C, D](implicit ev: ===[(A, B), (C, D)]): (ADPEval[C], ADPEval[D]) =
      (PairFstEq(ev).subst[ADPEval](a), PairSndEq(ev).subst[ADPEval](b))

    override def loss: APLoss[(A, B)] = l

    override def eval: (A, B) = (a.eval, b.eval)
  }

  def ArrEval[A, B, AL, BL](forward: ADPEval[A] => (ADPEval[B], BL => AL))(implicit al: APLoss.Aux[A, AL], bl: APLoss.Aux[B, BL]) =
    new ADPEval[A => B] {
      override def aeval[C, D](c: ADPEval[C])(
        implicit ev: (A => B) === (C => D), CL: APLoss[C], DL: APLoss[D]): (ADPEval[D], DL.loss => CL.loss) = {
        val ac = ArrDomEq(ev)
        val bd = ArrRngEq(ev)
        val f = forward(symm[Nothing, Any, A, C](ac).subst[ADPEval](c))
        (bd.subst[ADPEval](f._1), dl =>
          witness(symm[Nothing, Any, CL.loss, AL](CL.unique(al)(symm[Nothing, Any, A, C](ac))))(
            f._2(witness(DL.unique(bl)(symm[Nothing, Any, B, D](bd)))(dl))))
      }

      override def deval(implicit ev: ===[A => B, Double]): Double = throw new Exception("not DEval")

      override def loss: APLoss[A => B] = arrLoss(al, bl)

      override def peval[C, D](implicit ev: ===[(A) => B, (C, D)]): (ADPEval[C], ADPEval[D]) = throw new Exception("not PEval")

      override def eval: A => B = a => aeval[A, B](al.conv(a))._1.eval
    }

  case class DEval(d: Double) extends ADPEval[Double] {
    val eval = d

    override def aeval[A, B](a: ADPEval[A])(implicit ev: ===[Double, A => B], AL: APLoss[A], BL: APLoss[B]):
    (ADPEval[B], BL.loss => AL.loss) = throw new Exception("not ArrEval")

    override def peval[A, B](implicit ev: ===[Double, (A, B)]): (ADPEval[A], ADPEval[B]) = throw new Exception("not PEval")

    override def deval(implicit ev: ===[Double, Double]): Double = d

    override def loss: APLoss[Double] = dLoss
  }

}
