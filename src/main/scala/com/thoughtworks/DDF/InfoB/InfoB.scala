package com.thoughtworks.DDF.InfoB

//Short hand for InfoBase
trait InfoB[Info[_], Repr[_]] {
  def ReprInfo[A]: Repr[A] => Info[A]
}
