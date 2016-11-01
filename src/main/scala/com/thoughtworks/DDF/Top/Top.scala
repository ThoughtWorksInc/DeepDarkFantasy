package com.thoughtworks.DDF.Top

trait Top[Info[_], Repr[_]] extends TopInfo[Info, Repr] {
  def mkUnit: Repr[scala.Unit]
}
