package com.thoughtworks.DDF.Example

import com.thoughtworks.DDF.Language.{EvalMInterLang, InterLangTermLang}
import com.thoughtworks.DDF.NoInfo

object HelloWorld {
  def main(args: Array[String]): Unit = {
    import InterLangTermLang._
    val exp = litString("Hello World")
    println(exp[NoInfo, Lambda[X => X]](EvalMInterLang))
  }
}
