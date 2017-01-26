package com.thoughtworks.DDF.Example

import com.thoughtworks.DDF.Gradient.GDouble
import com.thoughtworks.DDF.Language._
import com.thoughtworks.DDF.{NoInfo, Show}

object Solve {
  def main(args: Array[String]): Unit = {
    val l = InterLangTermLang

    val nl = NextLang(l, l.doubleInfo)
    val exp = nl.collapse(nl.plusD__(nl.multD__(nl.in)(nl.in))(nl.plusD__(nl.multD__(nl.litD(2))(nl.in))(nl.litD(3))))
    val loss = nl.collapse(nl.Let__(nl.minusD__(nl.litD(27))(nl.in))(nl.W_(nl.multD)))

    val train = l.B__(loss)(exp)
    var weight: Double = 0
    val train_it: LangTerm[((Double, Double)) => (Double, Double)] =
      train(ADEvalInterLang).get[Double](
        ADEvalInterLang.aInfo(ADEvalInterLang.doubleInfo, ADEvalInterLang.doubleInfo))(GDouble)
    println(train_it[NoInfo, Lambda[X => Show]](ShowLang))
    for (_ <- Range(0, 100)) {
      for (_ <- Range(0, 2)) {
        weight -= 0.01 * train_it(InterLangTermLang)[NoInfo, Lambda[X => X]](EvalMInterLang)((weight, 1))._2
      }
      println(weight)
    }
  }
}
