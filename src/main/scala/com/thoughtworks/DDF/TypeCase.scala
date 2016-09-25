package com.thoughtworks.DDF

import scalaz.Leibniz._

trait TypeCase[Self[Y] <: TypeCase[Self, Y], X] {
  type ret

  def unique[Y](tc: Self[Y])(implicit ev: X === Y): ret === tc.ret = /*enforce by user*/ force[Nothing, Any, ret, tc.ret]
}
