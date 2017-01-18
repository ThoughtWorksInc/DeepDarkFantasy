package com.thoughtworks.DDF.Arrow

trait ArrInfo[Info[_], Repr[_]] {
  implicit def aInfo[A : Info, B : Info] : Info[A => B]
}
