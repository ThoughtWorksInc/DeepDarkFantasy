package com.thoughtworks.DDF.Arrow

trait Arr extends ArrType {
  def app[A, B]: A ~>: B => A => B
}
