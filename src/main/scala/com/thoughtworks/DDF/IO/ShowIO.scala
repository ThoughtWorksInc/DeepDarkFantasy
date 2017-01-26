package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.Double.ShowDoubleMin
import com.thoughtworks.DDF.Top.ShowTop
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowIO extends IO[NoInfo, Lambda[X => Show]] with SimpleIO[Lambda[X => Show]] with ShowDoubleMin with ShowTop {
  override def putDouble = Show("putDouble")

  override def IOBind[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("IOBind")

  override def getDouble = Show("getDouble")

  override def IORet[A](implicit ai: NoInfo[A]) = Show("IORet")
}

object ShowIO extends ShowIO
