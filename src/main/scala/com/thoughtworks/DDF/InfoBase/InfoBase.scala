package com.thoughtworks.DDF.InfoBase

//Short hand for InfoBase
trait InfoBase[Info[_], Repr[_]] {
  def reprInfo[A]: Repr[A] => Info[A]
}
