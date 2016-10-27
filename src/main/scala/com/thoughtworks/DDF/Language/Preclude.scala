package com.thoughtworks.DDF.Language

object Preclude {
  def square[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[Double => Double] = lang.W_(lang.multD)

  def square_[Info[_], Repr[_]](r: Repr[Double])(implicit lang: Lang[Info, Repr]): Repr[Double] =
    lang.app(square(lang))(r)

  def sumList[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[List[Double] => Double] = {
    val nLang = NextLang(lang, lang.listInfo(lang.doubleInfo))
    nLang.collapse(sumList_[nLang.info, nLang.repr](nLang.in)(nLang))
  }

  def sumList_[Info[_], Repr[_]](li: Repr[List[Double]])(implicit lang: Lang[Info, Repr]) = {
    import lang._
    foldLeft___(plusD)(litD(0))(li)
  }

  def dot__[Info[_], Repr[_]](l: Repr[List[Double]])(r: Repr[List[Double]])(implicit lang: Lang[Info, Repr]):
  Repr[Double] = {
    import lang._
    sumList_(listMap__(uncurry_(multD))(listZip__(l)(r)))
  }

  def dot_[Info[_], Repr[_]](l: Repr[List[Double]])(implicit lang: Lang[Info, Repr]): Repr[List[Double] => Double] = {
    val nLang = NextLang(lang, lang.listInfo(lang.doubleInfo))
    nLang.collapse(dot__[nLang.info, nLang.repr](nLang.rconv(l))(nLang.in)(nLang))
  }

  def dot[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[List[Double] => List[Double] => Double] = {
    val nLang = NextLang(lang, lang.listInfo(lang.doubleInfo))
    nLang.collapse(dot_[nLang.info, nLang.repr](nLang.in)(nLang))
  }
}