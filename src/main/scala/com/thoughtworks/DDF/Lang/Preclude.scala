package com.thoughtworks.DDF.Lang

object Preclude {
  def Square[Info[_], Repr[_]](implicit lang: Lang[Info, Repr], td: Info[Double]): Repr[Double => Double] = {
    val next = NextLang.apply[Info, Repr, Double](lang, td)
    import next._
    collapse(app(app(MultD)(in))(in))
  }
}
