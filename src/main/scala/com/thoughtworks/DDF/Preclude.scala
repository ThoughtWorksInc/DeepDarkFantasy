package com.thoughtworks.DDF

import Next.NextLanguage
import scala.language.higherKinds

object Preclude {
  def Square[Info[_], Repr[_]](implicit td: Info[Double], lang: Language[Info, Repr]): Repr[Double => Double] = {
    val next = NextLanguage[Info, Repr, Double](lang)
    import next._
    app(app(MultD)(in))(in)
  }

}
