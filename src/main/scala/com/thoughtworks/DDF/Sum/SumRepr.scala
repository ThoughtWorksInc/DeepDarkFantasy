package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait SumRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  def left[A, B](implicit ai: Info[A], bi: Info[B]): Repr[A => Either[A, B]]

  def right[A, B](implicit ai: Info[A], bi: Info[B]): Repr[B => Either[A, B]]

  def sumMatch[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[Either[A, B] => (A => C) => (B => C) => C]

  implicit def sumInfo[A, B](implicit ai: Info[A], bi: Info[B]): Info[Either[A, B]]

  def sumLeftInfo[A, B]: Info[Either[A, B]] => Info[A]

  def sumRightInfo[A, B]: Info[Either[A, B]] => Info[B]
}
