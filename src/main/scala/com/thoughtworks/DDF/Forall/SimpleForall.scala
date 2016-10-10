package com.thoughtworks.DDF.Forall

import com.thoughtworks.DDF.NoInfo

trait SimpleForall[Repr[_]] extends ForallRepr[NoInfo, Repr] {
  override def forallInfo[F[_]](implicit fi: FInfo[F]) = NoInfo()
}
