package com.thoughtworks.DDF.InfoBase

trait InfoBase[Info[_], Repr[_]] {
  def reprInfo[A]: Repr[A] => Info[A]
}
