package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bot.ADEvalBotMin
import com.thoughtworks.DDF.Combinators.ADEvalComb
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.{ADEval, ADEvalCase, ADEvalMatch}
import com.thoughtworks.DDF.IO.ADEvalIO
import com.thoughtworks.DDF.List.ADEvalList
import com.thoughtworks.DDF.Option.ADEvalOption
import com.thoughtworks.DDF.Product.ADEvalProd
import com.thoughtworks.DDF.Stream.ADEvalStream
import com.thoughtworks.DDF.Sum.ADEvalSum
import com.thoughtworks.DDF.Top.ADEvalTop

trait ADEvalInterLang extends
  InterLang[ADEvalCase, ADEval] with
  ADEvalProd with
  ADEvalList with
  ADEvalComb with
  ADEvalSum with
  ADEvalOption with
  ADEvalBotMin with
  ADEvalIO with
  ADEvalTop with
  ADEvalStream {
  override val base = LangTermLang
  override def stringInfo: ADEvalCase.Aux[String, Lambda[X => String]] = new ADEvalCase[String] {
    override def wgi[G: Gradient] = base.stringInfo

    override type WithGrad[_] = String

    override def tmr = ()

    override val tm = sem
  }

  override def litString: String => ADEval[String] = str =>
    new ADEval[String] {
      override def term[G: Gradient] = base.litString(str)

      override val fec = stringInfo
    }

  def sem[A, B] = new ADEvalMatch[String] {
    override type ret = Unit
  }
}

object ADEvalInterLang extends ADEvalInterLang