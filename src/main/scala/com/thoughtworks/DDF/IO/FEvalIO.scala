package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.Double.FEvalDouble
import com.thoughtworks.DDF.Language.{LangInfoG, LangTermLang}
import com.thoughtworks.DDF.Top.FEvalTop
import com.thoughtworks.DDF.{FEMMatch, FEval, FEvalCase, Gradient}

trait FEvalIO[G] extends
  IO[FEvalCase[G, ?], FEval[G, ?]] with
  FEvalDouble[G] with
  FEvalTop[G] {
  override val base = LangTermLang

  override def putDouble: FEval[G, Double => IO[Unit]] = new FEval[G, Double => IO[Unit]] {
    override val tm = aInfo(doubleInfo, IOInfo(topInfo))

    override val deriv = base.B__(base.putDouble)(base.zro(base.doubleInfo, grad.GInfo))
  }

  override def IOBind[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, IO[A] => (A => IO[B]) => IO[B]] {
      override val tm = aInfo(IOInfo(ai), aInfo(aInfo(ai, IOInfo(bi)), IOInfo(bi)))

      override val deriv = base.IOBind(ai.lr, bi.lr)
    }

  override def IORet[A](implicit ai: FEvalCase[G, A]): FEval[G, A => IO[A]] =
    new FEval[G, A => IO[A]] {
      override val tm = aInfo(ai, IOInfo(ai))

      override val deriv = base.IORet(ai.lr)
    }

  override def getDouble: FEval[G, IO[Double]] =
    new FEval[G, IO[Double]] {
      override val tm = IOInfo(doubleInfo)

      override val deriv =
        base.IOBind__(
          base.getDouble)(
          base.B__(base.IORet(doubleInfo.lr))(base.C__(base.mkProd(base.doubleInfo, grad.GInfo))(grad.constG)))
    }

  def iofem[A] = new FEMMatch[G, IO[A]] {
    override type ret = FEvalCase[G, A]
  }

  override def IOInfo[A](implicit ai: FEvalCase[G, A]): FEvalCase.Aux[G, IO[A], IO[ai.ret]] =
    new FEvalCase[G, IO[A]] {
      override def lr: LangInfoG[IO[ai.ret]] = base.IOInfo(ai.lr)

      override type ret = IO[ai.ret]

      override val tm = iofem[A]

      override def tmr: tm.ret = ai
    }

  override def IOElmInfo[A]: FEvalCase[G, IO[A]] => FEvalCase[G, A] = _.get(iofem[A])
}

object FEvalIO {
  implicit def apply[G](implicit g: Gradient[G]): FEvalIO[G] = new FEvalIO[G] {
    override val grad: Gradient[G] = g
  }
}
