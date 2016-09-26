package com.thoughtworks.DDF

trait SumLang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  def left[A, B](implicit at: Info[A], bt: Info[B]): Repr[A => Either[A, B]]

  def right[A, B](implicit at: Info[A], bt: Info[B]): Repr[B => Either[A, B]]

  def sumMatch[A, B, C](implicit at: Info[A], bt: Info[B], ct: Info[C]): Repr[(A => C) => (B => C) => (Either[A, B] => C)]

  def SumInfo[A, B]: Info[A] => Info[B] => Info[Either[A, B]]

  def SumLeftInfo[A, B]: Info[Either[A, B]] => Info[A]

  def SumRightInfo[A, B]: Info[Either[A, B]] => Info[B]
}
