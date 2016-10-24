package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arrow

trait Y[Info[_], Repr[_]] extends Arrow[Info, Repr] {
  def Y[A, B](implicit ai: Info[A], bi: Info[B]): Repr[((A => B) => (A => B)) => (A => B)]
}
