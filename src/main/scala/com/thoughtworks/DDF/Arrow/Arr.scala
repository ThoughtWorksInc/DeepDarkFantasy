package com.thoughtworks.DDF.Arrow

trait Arr extends ArrType {
  def app[A <: Type: Kind, B <: Type: Kind](f: A ~>: B)(x: A): B
}
