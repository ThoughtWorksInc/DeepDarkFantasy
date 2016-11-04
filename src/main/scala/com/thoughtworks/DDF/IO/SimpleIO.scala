package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.Arrow.SimpleArr
import com.thoughtworks.DDF.NoInfo

trait SimpleIO[Repr[_]] extends IOInfo[NoInfo, Repr] with SimpleArr[Repr] {
  override def IOInfo[A](implicit ai: NoInfo[A]): NoInfo[IO[A]] = NoInfo()

  override def IOElmInfo[A]: NoInfo[IO[A]] => NoInfo[A] = _ => NoInfo()
}

object SimpleIO {
  implicit def apply[Repr[_]]: SimpleIO[Repr] = new SimpleIO[Repr] { }
}