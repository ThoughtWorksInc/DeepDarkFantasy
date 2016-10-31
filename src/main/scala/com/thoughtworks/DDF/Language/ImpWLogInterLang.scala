package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.{BEval, ImpW, ImpWLog, Loss}

import scalaz.NaturalTransformation

trait ImpWLogInterLang extends NTInterLang[InterLangInfoG, ImpW, ImpWLog] {
  override def base: InterLang[InterLangInfoG, ImpW] = ImpWInterLang

  def defLogHandler: Seq[String] => Seq[String]

  override def NTF: NaturalTransformation[ImpW, ImpWLog] = new NaturalTransformation[ImpW, ImpWLog] {
    override def apply[A](fa: ImpW[A]): ImpWLog[A] = new ImpWLog[A] {
      override val exp: ImpW[A] = fa

      override def forward: Forward = new Forward {
        override def update(rate: Double)(tloss: Loss[A]): ImpWLog[A] =
          NTF(fres.update(rate, tloss))

        val fres = exp.forward

        override val res: BEval[A] = fres.res

        override val log: Seq[String] = Seq(fres.res.toString)
      }
    }
  }

  override def app[A, B]: ImpWLog[A => B] => ImpWLog[A] => ImpWLog[B] = f => x =>
    new ImpWLog[B] {
      override def forward: Forward = new Forward {
        override def update(rate: Double)(tloss: Loss[B]): ImpWLog[B] = {
          val al = tem.backward(tloss)
          app(ff.update(rate)(BEvalInterLang.lossA(xf.res)(tloss)))(xf.update(rate)(al))
        }

        val ff = f.forward

        val xf = x.forward

        val tem = BEvalInterLang.aeval(ff.res).forward(xf.res)

        override val res: BEval[B] = tem.eb

        override val log: Seq[String] = ff.log ++ xf.log
      }

      val expRich: ImpW.Aux[B, (f.exp.Weight, x.exp.Weight)] = ImpWInterLang.appRich(f.exp)(x.exp)

      override val exp: ImpW[B] = expRich
    }

  override def reprInfo[A]: ImpWLog[A] => InterLangInfoG[A] = _.exp.ti
}

object ImpWLogInterLang {
  def apply(h: Seq[String] => Seq[String]) = new ImpWLogInterLang {
    override def defLogHandler: Seq[String] => Seq[String] = h
  }
}