package com.thoughtworks.DDF.Except

import com.thoughtworks.DDF.Sum.SumMin

trait ExceptBase extends SumMin {
  final type Except[+A <: Type, +B <: Type] = Sum[A, B]
}
