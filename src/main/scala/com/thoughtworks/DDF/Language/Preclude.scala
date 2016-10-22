package com.thoughtworks.DDF.Language

object Preclude {
  def square[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[Double => Double] = {
    val next = NextLang.apply[Info, Repr, Double](lang, lang.doubleInfo)
    import next._
    collapse(app(app(multD)(in))(in))
  }

  def sumList[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[List[Double] => Double] = {
    val nLang = NextLang.apply[Info, Repr, List[Double]](lang, lang.listInfo(lang.doubleInfo))
    import nLang._
    collapse(app(app(app(foldLeft[Double, Double])(plusD))(litD(0)))(in))
  }

  def dot[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[List[Double] => List[Double] => Double] = {
    val nLang = NextLang.apply[Info, Repr, List[Double]](lang, lang.listInfo(lang.doubleInfo))
    val nNLang =
      NextLang.apply[
        Lambda[X => Info[List[Double] => X]],
        Lambda[X => Either[Repr[X], Repr[List[Double] => X]]],
        List[Double]](nLang, nLang.listInfo(nLang.doubleInfo))
    import nNLang._
    nLang.collapse(collapse(app(sumList[
      Lambda[X => Info[List[Double] => List[Double] => X]],
      Lambda[X => Either[
        Either[Repr[X], Repr[List[Double] => X]],
        Either[Repr[List[Double] => X], Repr[List[Double] => List[Double] => X]]]]](nNLang))(
      app(
        app(listMap[(Double, Double), Double])(app(uncurry[Double, Double, Double])(multD)))
      (app(app(listZip[Double, Double])(rconv(nLang.in)))(in)))))
  }
}