package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.LangBase.LangBase

trait SumType extends LangBase {
  type Sum[+A <: Type, +B <: Type] <: Type

  implicit def SumK[A <: Type: Kind, B <: Type: Kind]: Kind[Sum[A, B]]
}
