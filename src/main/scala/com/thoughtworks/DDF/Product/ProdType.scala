package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.LangBase.LangBase

trait ProdType extends LangBase {
  type Prod[+A <: Type, +B <: Type] <: Type

  implicit def ProdK[A <: Type: Kind, B <: Type: Kind]: Kind[Prod[A, B]]
}
