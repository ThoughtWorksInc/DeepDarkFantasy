package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.Arr

trait ITE extends Arr with BoolType {
  def ite[A <: Type: Kind]: Bool ~>: A ~>: A ~>: A

  def ite_[A <: Type: Kind](b: Bool) = app(ite)(b)

  def ite__[A <: Type: Kind](b: Bool)(l: A) = app(ite_(b))(l)

  def ite___[A <: Type: Kind](b: Bool)(l: A)(r: A) = app(ite__(b)(l))(r)
}
