package com.thoughtworks.DDF.InfoB

//Short hand for InfoBase
trait InfoB[Info[_], Repr[_]] {
  def reprInfo[A]: Repr[A] => Info[A]
}
