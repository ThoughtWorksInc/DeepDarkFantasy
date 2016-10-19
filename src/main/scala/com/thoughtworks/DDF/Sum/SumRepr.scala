package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait SumRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] with SumInfo[Info, Repr] {
  def left[A, B](implicit ai: Info[A], bi: Info[B]): Repr[A => Either[A, B]]

  def right[A, B](implicit ai: Info[A], bi: Info[B]): Repr[B => Either[A, B]]

  def sumMatch[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[Either[A, B] => (A => C) => (B => C) => C]

  def sumComm[A, B](implicit ai: Info[A], bi: Info[B]): Repr[Either[A, B] => Either[B, A]]

  def sumAssocLR[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]):
  Repr[Either[Either[A, B], C] => Either[A, Either[B, C]]]

  def sumAssocRL[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]):
  Repr[Either[A, Either[B, C]] => Either[Either[A, B], C]]
}
