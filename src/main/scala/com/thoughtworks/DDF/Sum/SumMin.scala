package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait SumMin[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] with SumInfo[Info, Repr] {
  def left[A, B](implicit ai: Info[A], bi: Info[B]): Repr[A => Either[A, B]]

  def right[A, B](implicit ai: Info[A], bi: Info[B]): Repr[B => Either[A, B]]

  def sumMatch[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[Either[A, B] => (A => C) => (B => C) => C]
}
