package com.thoughtworks.DDF.Top

trait TopExt[Info[_], Repr[_]] extends Top[Info, Repr] {
  val unit: Top[Info, Repr]

  override def mkUnit = unit.mkUnit

  override implicit def unitInfo = unit.unitInfo

  override def reprInfo[A] = unit.reprInfo[A]
}

object TopExt {
  implicit def apply[Info[_], Repr[_]](implicit un: Top[Info, Repr]): Top[Info, Repr] = new TopExt[Info, Repr] {
    override val unit: Top[Info, Repr] = un
  }
}