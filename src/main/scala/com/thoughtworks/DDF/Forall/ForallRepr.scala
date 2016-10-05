package com.thoughtworks.DDF.Forall

import com.thoughtworks.DDF.Arrow.ArrowRepr

import scalaz.Forall

trait ForallRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  def specialize[F[_], A](implicit ai: Info[A]): Repr[Forall[F] => F[A]]
}
