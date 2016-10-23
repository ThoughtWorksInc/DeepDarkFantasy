package com.thoughtworks.DDF.Unit

trait UnitExt[Info[_], Repr[_]] extends Unit[Info, Repr] {
  val unit: Unit[Info, Repr]

  override def mkUnit = unit.mkUnit

  override implicit def unitInfo = unit.unitInfo

  override def reprInfo[A] = unit.reprInfo[A]
}

object UnitExt {
  implicit def apply[Info[_], Repr[_]](implicit un: Unit[Info, Repr]): Unit[Info, Repr] = new UnitExt[Info, Repr] {
    override val unit: Unit[Info, Repr] = un
  }
}