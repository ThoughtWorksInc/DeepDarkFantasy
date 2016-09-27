package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arr.ArrLang
import com.thoughtworks.DDF.Unit.UnitLang

trait ListLang[Info[_], Repr[_]] extends UnitLang[Info, Repr] with ArrLang[Info, Repr] {
  implicit def ListInfo[A](implicit ai : Info[A]) : Info[List[A]]
  def ListElmInfo[A](implicit lai : Info[List[A]]) : Info[A]
  def Nil[A](implicit ai : Info[A]) : Repr[List[A]]
  def Cons[A] : Repr[A] => Repr[List[A]] => Repr[List[A]]
  def listMatch[A, B](implicit bi : Info[B]) : Repr[Unit => B] => Repr[A => List[A] => B] => Repr[List[A]] => Repr[B]
}