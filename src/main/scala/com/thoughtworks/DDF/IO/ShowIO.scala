package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.Double.ShowDoubleMin
import com.thoughtworks.DDF.Top.ShowTop
import com.thoughtworks.DDF.{NoInfo, ShowLeaf}

trait ShowIO extends IO[NoInfo, ShowLeaf] with SimpleIO[ShowLeaf] with ShowDoubleMin with ShowTop {
  override def putDouble = ShowLeaf("putDouble")

  override def IOBind[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = ShowLeaf("IOBind")

  override def getDouble = ShowLeaf("getDouble")

  override def IORet[A](implicit ai: NoInfo[A]) = ShowLeaf("IORet")
}

object ShowIO extends ShowIO
