package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.ArrInfo
import com.thoughtworks.DDF.Top.TopInfo

trait StreamInfo[Info[_], Repr[_]] extends TopInfo[Info, Repr] with ArrInfo[Info, Repr] {
  implicit def streamInfo[A](implicit ai: Info[A]): Info[scala.Stream[A]]

  def streamElmInfo[A]: Info[scala.Stream[A]] => Info[A]
}
