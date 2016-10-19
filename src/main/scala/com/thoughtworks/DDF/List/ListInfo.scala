package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.ArrowInfo

trait ListInfo[Info[_], Repr[_]] extends ArrowInfo[Info, Repr] {
  implicit def listInfo[A](implicit ai: Info[A]): Info[List[A]]

  def listElmInfo[A](implicit lai: Info[List[A]]): Info[A]
}
