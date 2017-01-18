package com.thoughtworks.DDF.Top

trait TopInfo[Info[_], Repr[_]] {
  implicit def topInfo: Info[Unit]
}
