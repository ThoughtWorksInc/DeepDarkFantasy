package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.ArrInfo

trait ListInfo[Info[_], Repr[_]] extends ArrInfo[Info, Repr] {
  implicit def listInfo[A](implicit ai: Info[A]): Info[scala.List[A]]

  def listElmInfo[A]: Info[scala.List[A]] => Info[A]
}
