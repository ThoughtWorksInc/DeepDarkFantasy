package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.Double.FEvalDouble
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTermLang}
import com.thoughtworks.DDF.Top.FEvalTop
import com.thoughtworks.DDF.{FEval, FEvalCase, FEvalMatch}

trait FEvalIO extends
  IO[FEvalCase, FEval] with
  FEvalDouble with
  FEvalTop {
  override val base = LangTermLang

  override def putDouble: FEval[Double => IO[Unit]] = new FEval[Double => IO[Unit]] {
    override val fec = aInfo(doubleInfo, IOInfo(topInfo))

    override def term[G: Gradient] = base.B__(base.putDouble)(base.zro(base.doubleInfo, implicitly[Gradient[G]].GInfo))
  }

  override def IOBind[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[IO[A] => (A => IO[B]) => IO[B]] {
      override val fec = aInfo(IOInfo(ai), aInfo(aInfo(ai, IOInfo(bi)), IOInfo(bi)))

      override def term[G: Gradient] = base.IOBind(ai.wgi[G], bi.wgi[G])
    }

  override def IORet[A](implicit ai: FEvalCase[A]): FEval[A => IO[A]] =
    new FEval[A => IO[A]] {
      override val fec = aInfo(ai, IOInfo(ai))

      override def term[G: Gradient] = base.IORet(ai.wgi[G])
    }

  override def getDouble: FEval[IO[Double]] =
    new FEval[IO[Double]] {
      override val fec = IOInfo(doubleInfo)

      override def term[G: Gradient] =
        base.IOBind__(
          base.getDouble)(
          base.B__(base.IORet(doubleInfo.wgi[G]))(
            base.C__(base.mkProd(base.doubleInfo, implicitly[Gradient[G]].GInfo))(implicitly[Gradient[G]].constG)))
    }

  def iofem[A] = new FEvalMatch[IO[A]] {
    override type ret = FEvalCase[A]
  }

  override def IOInfo[A](implicit ai: FEvalCase[A]): FEvalCase.Aux[IO[A], Lambda[G => IO[ai.WithGrad[G]]]] =
    new FEvalCase[IO[A]] {
      override def wgi[G: Gradient]: LangInfoG[IO[ai.WithGrad[G]]] = base.IOInfo(ai.wgi[G])

      override type WithGrad[G] = IO[ai.WithGrad[G]]

      override val tm = iofem[A]

      override def tmr: tm.ret = ai
    }

  override def IOElmInfo[A]: FEvalCase[IO[A]] => FEvalCase[A] = _.get(iofem[A])
}

object FEvalIO extends FEvalIO