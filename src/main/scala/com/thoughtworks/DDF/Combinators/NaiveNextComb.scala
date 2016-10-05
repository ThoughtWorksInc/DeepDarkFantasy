package com.thoughtworks.DDF.Combinators

trait NaiveNextComb[Info[_], Repr[_], Arg] extends
  Comb[Lambda[X => Info[Arg => X]], Lambda[X => Repr[Arg => X]]] with
  NaiveNextSKI[Info, Repr, Arg] {
  implicit def base: Comb[Info, Repr]

  override def Y[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) = rconv(base.Y[A, B](convi(ai), convi(bi)))

  override def B[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]) =
    rconv(base.B[A, B, C](convi(ai), convi(bi), convi(ci)))

  override def W[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) = rconv(base.W[A, B](convi(ai), convi(bi)))

  override def C[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]) =
    rconv(base.C[A, B, C](convi(ai), convi(bi), convi(ci)))

  override def Let[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) = rconv(base.Let(convi(ai), convi(bi)))

  override def App[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) = rconv(base.App(convi(ai), convi(bi)))
}

object NaiveNextComb {
  implicit def apply[Info[_], Repr[_], Arg](implicit cu: Comb[Info, Repr], arg: Info[Arg]) =
    new NaiveNextComb[Info, Repr, Arg] {
      override implicit def base: Comb[Info, Repr] = cu

      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKIRepr[Info, Repr] = cu
    }
}
