package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.{NoInfo, ShowLeaf}

trait ShowBool extends Bool[NoInfo, ShowLeaf] with SimpleBool[ShowLeaf] with ShowArr {
  override def litB = b => ShowLeaf(b.toString)

  override def ite[A](implicit ai: NoInfo[A]) = ShowLeaf("ite")
}

object ShowBool extends ShowBool