package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.ArrowRepr
import com.thoughtworks.DDF.Unit.UnitRepr

trait ListRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  implicit def listInfo[A](implicit ai: Info[A]): Info[List[A]]

  def listElmInfo[A](implicit lai: Info[List[A]]): Info[A]

  def nil[A](implicit ai: Info[A]): Repr[List[A]]

  def cons[A](implicit ai: Info[A]): Repr[A => List[A] => List[A]]

  def listMatch[A, B](implicit ai: Info[A], bi: Info[B]): Repr[List[A] => B => (A => List[A] => B) => B]

  def listMap[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => B) => List[A] => List[B]]
}