package com.thoughtworks.DDF.Combinators

trait NaiveNextComb[Info[_], Repr[_], Arg] extends
  Comb[Lambda[X => Info[Arg => X]], Lambda[X => Repr[Arg => X]]] with
  NaiveNextSKI[Info, Repr, Arg] {
  implicit def base: Comb[Info, Repr]

  override def Y[A, B](implicit at: Info[Arg => A], bt: Info[Arg => B]):
  Repr[Arg => ((A => B) => A => B) => A => B] = rconv(base.Y[A, B](base.arrowRangeInfo(at), base.arrowRangeInfo(bt)))

  override def B[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]):
  Repr[Arg => (B => C) => (A => B) => A => C] =
    rconv(base.B[A, B, C](base.arrowRangeInfo(ai), base.arrowRangeInfo(bi), base.arrowRangeInfo(ci)))

  override def W[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]): Repr[Arg => (A => A => B) => A => B] =
    rconv(base.W[A, B](base.arrowRangeInfo(ai), base.arrowRangeInfo(bi)))

  override def C[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]):
  Repr[Arg => (A => B => C) => B => A => C] =
    rconv(base.C[A, B, C](base.arrowRangeInfo(ai), base.arrowRangeInfo(bi), base.arrowRangeInfo(ci)))
}

object NaiveNextComb {
  implicit def apply[Info[_], Repr[_], Arg](implicit cu: Comb[Info, Repr], arg: Info[Arg]) =
    new NaiveNextComb[Info, Repr, Arg] {
      override implicit def base: Comb[Info, Repr] = cu

      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKIRepr[Info, Repr] = cu
    }
}
