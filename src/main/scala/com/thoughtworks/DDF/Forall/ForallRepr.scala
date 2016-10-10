package com.thoughtworks.DDF.Forall

import com.thoughtworks.DDF.Arrow.ArrowRepr

import scalaz.{Forall, NaturalTransformation}

trait ForallRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  type FInfo[F[_]] = NaturalTransformation[Info, Lambda[X => Info[F[X]]]]

  def forallInfo[F[_]](implicit fi: FInfo[F]): Info[Forall[F]]

  def specialize[F[_], A](implicit ai: Info[A], fi: FInfo[F]): Repr[Forall[F] => F[A]]
}
