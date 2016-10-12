package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.ArrowInfo

trait SumInfo[Info[_], Repr[_]] extends ArrowInfo[Info, Repr] {
  implicit def sumInfo[A, B](implicit ai: Info[A], bi: Info[B]): Info[Either[A, B]]

  def sumLeftInfo[A, B]: Info[Either[A, B]] => Info[A]

  def sumRightInfo[A, B]: Info[Either[A, B]] => Info[B]
}
