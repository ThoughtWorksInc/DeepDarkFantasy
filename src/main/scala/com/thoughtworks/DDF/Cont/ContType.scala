package com.thoughtworks.DDF.Cont

import com.thoughtworks.DDF.Arrow.ArrType

trait ContType extends ArrType {
  final type Cont[-R <: Type, -A <: Type] = (A ~>: R) ~>: R

  def ContK[R <: Type: Kind, A <: Type: Kind]: Kind[Cont[R, A]]
}
