package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.SimpleArr
import com.thoughtworks.DDF.NoInfo
import com.thoughtworks.DDF.Top.SimpleTop

trait SimpleStream[Repr[_]] extends StreamType[NoInfo, Repr] with SimpleArr[Repr] with SimpleTop[Repr] {
  override implicit def streamInfo[A](implicit ai: NoInfo[A]): NoInfo[scala.Stream[A]] = NoInfo()

  override def streamElmInfo[A]: NoInfo[scala.Stream[A]] => NoInfo[A] = _ => NoInfo()
}

object SimpleStream {
  implicit def apply[Repr[_]] = new SimpleStream[Repr] { }
}