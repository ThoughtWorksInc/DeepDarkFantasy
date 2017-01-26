package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.{NoInfo, ShowLeaf}

trait ShowArr extends Arr[NoInfo, ShowLeaf] with SimpleArr[ShowLeaf] {
  override def app[A, B] = f => x => ShowLeaf("(" + f.s + " " + x.s + ")")
}

object ShowArr extends ShowArr