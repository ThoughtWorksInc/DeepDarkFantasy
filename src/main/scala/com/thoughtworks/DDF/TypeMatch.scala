package com.thoughtworks.DDF

import scalaz.Leibniz._

trait TypeMatch[Self[Y] <: TypeMatch[Self, Y], X] {
  type ret

  def unique(tc: Self[X]): ret === tc.ret = /*enforce by user*/ force[Nothing, Any, ret, tc.ret]
}
