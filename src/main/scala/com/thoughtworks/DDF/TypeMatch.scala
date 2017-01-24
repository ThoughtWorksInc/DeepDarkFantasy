package com.thoughtworks.DDF

import scalaz.Leibniz._

trait TypeMatch[Self <: TypeMatch[Self]] {
  type ret

  def unique(tc: Self): ret === tc.ret = /*enforce by user*/ force[Nothing, Any, ret, tc.ret]
}
