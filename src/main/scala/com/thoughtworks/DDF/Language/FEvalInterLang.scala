package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bot.FEvalBotMin
import com.thoughtworks.DDF.Combinators.FEvalComb
import com.thoughtworks.DDF.Double.FEvalDouble
import com.thoughtworks.DDF.List.FEvalList
import com.thoughtworks.DDF.Option.FEvalOption
import com.thoughtworks.DDF.Product.FEvalProd
import com.thoughtworks.DDF.Sum.FEvalSum
import com.thoughtworks.DDF.Top.{FEvalTop, Top}
import com.thoughtworks.DDF.{FEMMatch, FEval, FEvalCase}

trait FEvalInterLang[G] extends
  InterLang[FEvalCase[G, ?], FEval[G, ?]] with
  FEvalProd[G] with
  FEvalList[G] with
  FEvalComb[G] with
  FEvalSum[G] with
  FEvalOption[G] with
  FEvalBotMin[G] with
  FEvalDouble[G] with
  FEvalTop[G] {
  override val base = LangTermLang

  override def putDouble: FEval[G, Double => IO[Unit]] = ???

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
        base.IOBind__[Double, (Double, G)](
          base.getDouble)(
          base.B__[Double, (Double, G), IO[(Double, G)]](base.IORet[(Double, G)](doubleInfo.lr))(???))
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

object FEvalInterLang {
  implicit def apply[G]: FEvalInterLang[G] = new FEvalInterLang[G] { }
}