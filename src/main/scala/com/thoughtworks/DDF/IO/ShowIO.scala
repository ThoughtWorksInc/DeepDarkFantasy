package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.Double.ShowDouble
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowIO extends IO[NoInfo, Show] with SimpleIO[Show] with ShowDouble {
  override def putDouble = Show("putDouble")

  override def IOBind[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("IOBind")

  override def getDouble = Show("getDouble")

  override def IORet[A](implicit ai: NoInfo[A]) = Show("IORet")
}

object ShowIO extends ShowIO
