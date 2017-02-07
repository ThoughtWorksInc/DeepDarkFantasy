package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.Double.ADEvalDoubleMin
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTermLang}
import com.thoughtworks.DDF.Top.ADEvalTop
import com.thoughtworks.DDF.{ADEval, ADEvalCase, ADEvalMatch}
import com.thoughtworks.DDF.RecursiveInfoMatch._

trait ADEvalIO extends
  IO[ADEvalCase, ADEval] with
  ADEvalDoubleMin with
  ADEvalTop {
  override val base = LangTermLang

  override def putDouble: ADEval[Double => IO[Unit]] = new ADEval[Double => IO[Unit]] {
    override val fec = aInfo(doubleInfo, IOInfo(topInfo))

    override def term[G: Gradient] = base.B__(base.putDouble)(base.zro(base.doubleInfo, implicitly[Gradient[G]].GInfo))
  }

  override def IOBind[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[IO[A] => (A => IO[B]) => IO[B]] {
      override val fec = aInfo(IOInfo(ai), aInfo(aInfo(ai, IOInfo(bi)), IOInfo(bi)))

      override def term[G: Gradient] = base.IOBind(ai.wgi[G], bi.wgi[G])
    }

  override def IORet[A](implicit ai: ADEvalCase[A]): ADEval[A => IO[A]] =
    new ADEval[A => IO[A]] {
      override val fec = aInfo(ai, IOInfo(ai))

      override def term[G: Gradient] = base.IORet(ai.wgi[G])
    }

  override def getDouble: ADEval[IO[Double]] =
    new ADEval[IO[Double]] {
      override val fec = IOInfo(doubleInfo)

      override def term[G: Gradient] =
        base.IOBind__(
          base.getDouble)(
          base.B__(base.IORet(doubleInfo.wgi[G]))(
            base.C__(base.mkProd(base.doubleInfo, implicitly[Gradient[G]].GInfo))(implicitly[Gradient[G]].constG)))
    }

  override def IOInfo[A](implicit ai: ADEvalCase[A]): ADEvalCase.Aux[IO[A], Lambda[G => IO[ai.WithGrad[G]]]] =
    new ADEvalCase[IO[A]] with IORI[ADEvalMatch, ADEvalCase, A] {
      override def wgi[G: Gradient]: LangInfoG[IO[ai.WithGrad[G]]] = base.IOInfo(ai.wgi[G])

      override type WithGrad[G] = IO[ai.WithGrad[G]]

      override def tmr = ai
    }

  override def IOElmInfo[A]: ADEvalCase[IO[A]] => ADEvalCase[A] = _.get(IOM[ADEvalMatch, ADEvalCase, A])
}

object ADEvalIO extends ADEvalIO