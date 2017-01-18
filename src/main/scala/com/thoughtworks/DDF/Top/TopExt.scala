package com.thoughtworks.DDF.Top

trait TopExt[Info[_], Repr[_]] extends Top[Info, Repr] {
  val unit: Top[Info, Repr]

  override def mkTop = unit.mkTop

  override implicit def topInfo = unit.topInfo
}

object TopExt {
  implicit def apply[Info[_], Repr[_]](implicit un: Top[Info, Repr]): Top[Info, Repr] = new TopExt[Info, Repr] {
    override val unit: Top[Info, Repr] = un
  }
}