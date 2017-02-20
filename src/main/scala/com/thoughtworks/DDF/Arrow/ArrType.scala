package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.LangBase.LangBase

trait ArrType extends LangBase {
  type ~>:[-Dom <: Type, +Rng <: Type] <: Type

  implicit def `~>:K`[A <: Type: Kind, B <: Type: Kind]: Kind[A ~>: B]
}
