package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.{BEval, ImpW, ImpWLog, Loss}

import scala.language.reflectiveCalls //TODO: no idea what this is for, find out later
import scalaz.NaturalTransformation

trait ImpWLogLang extends NTLang[LangInfoG, ImpW, ImpWLog] {
  override def base: Lang[LangInfoG, ImpW] = ImpWLang

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
          app(ff.update(rate)(BEvalLang.lossA(xf.res)(tloss)))(xf.update(rate)(al))
        }

        val ff = f.forward

        val xf = x.forward

        val tem = BEvalLang.aeval(ff.res).forward(xf.res)

        override val res: BEval[B] = tem.eb

        override val log: Seq[String] = ff.log ++ xf.log
      }

      val expRich: ImpW.Aux[B, (f.exp.Weight, x.exp.Weight)] = ImpWLang.appRich(f.exp)(x.exp)

      override val exp: ImpW[B] = expRich
    }

  override def reprInfo[A]: ImpWLog[A] => LangInfoG[A] = _.exp.ti
}

object ImpWLogLang {
  def apply(h: Seq[String] => Seq[String]) = new ImpWLogLang {
    override def defLogHandler: Seq[String] => Seq[String] = h
  }
}