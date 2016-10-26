package com.thoughtworks.DDF.Language

object Preclude {
  def square[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[Double => Double] = lang.W_(lang.multD)

  def sumList[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[List[Double] => Double] = {
    val nLang = NextLang(lang, lang.listInfo(lang.doubleInfo))
    import nLang._
    collapse(foldLeft___(plusD)(litD(0))(in))
  }

  def sumList_[Info[_], Repr[_]](li: Repr[List[Double]])(implicit lang: Lang[Info, Repr]) =
    lang.app(sumList)(li)

  def dot[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[List[Double] => List[Double] => Double] = {
    val nLang = NextLang(lang, lang.listInfo(lang.doubleInfo))
    val nNLang =
      NextLang.apply[nLang.info, nLang.repr, List[Double]](nLang, nLang.listInfo(nLang.doubleInfo))
    import nNLang._
    nLang.collapse(collapse(sumList_[nNLang.info, nNLang.repr](
      listMap__(uncurry_(multD))(listZip__(rconv(nLang.in))(in)))(nNLang)))
  }
}