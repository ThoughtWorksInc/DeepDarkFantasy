package com.thoughtworks.DDF

import Next.NextLanguage
import scala.language.higherKinds

object Preclude {
  def Square[Type[_], Repr[_]](implicit td: Type[Double], lang: Language[Type, Repr]): Repr[Double => Double] = {
    val next = NextLanguage[Type, Repr, Double](lang)
    import next._
    app(app(MultD)(in))(in)
  }

}
