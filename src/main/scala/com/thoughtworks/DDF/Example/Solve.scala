package com.thoughtworks.DDF.Example

import com.thoughtworks.DDF.Gradient.GDouble
import com.thoughtworks.DDF.Language._
import com.thoughtworks.DDF.NoInfo

object Solve {
  def main(args: Array[String]): Unit = {
    val l = InterLangTermLang

    val nl = NextLang(l, l.doubleInfo)
    val exp = nl.collapse(nl.plusD__(nl.multD__(nl.in)(nl.in))(nl.plusD__(nl.multD__(nl.litD(2))(nl.in))(nl.litD(3))))
    val loss = nl.collapse(nl.Let__(nl.minusD__(nl.litD(27))(nl.in))(nl.W_(nl.multD)))

    val train = l.B__(loss)(exp)
    var weight : Double = 0
    println(l.multD__(l.litD(100))(l.litD(200))(ADEvalInterLang).term[Double](GDouble)(ShowLang).s)
    //run, just very slow and large
    for (_ <- Range(0, 100)) {
      for (_ <- Range(0, 10)) {
        //loss(ADEvalInterLang).term[Double](GDouble)(InterLangTermLang)[NoInfo, Lambda[X => X]](EvalMInterLang)
      }
      //println(weight)
    }
  }
}
