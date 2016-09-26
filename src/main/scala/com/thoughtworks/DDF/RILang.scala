package com.thoughtworks.DDF

trait RILang[Info[_], Repr[_]] {
  def ReprInfo[A]: Repr[A] => Info[A]
}
