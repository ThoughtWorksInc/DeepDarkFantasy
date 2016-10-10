package com.thoughtworks.DDF.Forall

import com.thoughtworks.DDF.Arrow.NextArrow
import com.thoughtworks.DDF.Combinators.SKIRepr

import scalaz.{Forall, NaturalTransformation}

trait NextForall[Info[_], Repr[_], Arg] extends
  ForallRepr[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  NextArrow[Info, Repr, Arg] {
  override implicit def base: ForallRepr[Info, Repr]

  def convf[F[_]]: FInfo[F] => NaturalTransformation[Info, Lambda[X => Info[F[X]]]] = fi =>
    new NaturalTransformation[Info, Lambda[X => Info[F[X]]]] {
      override def apply[A](fa: Info[A]): Info[F[A]] = convi(fi.apply(iconv(fa)))
    }

  override def forallInfo[F[_]](implicit fi: FInfo[F]): Info[Arg => Forall[F]] = iconv(base.forallInfo[F](convf(fi)))

  override def specialize[F[_], A](implicit ai: Info[Arg => A], fi: FInfo[F]) =
    rconv(base.specialize[F, A](convi(ai), convf(fi)))
}

object NextForall {
  implicit def apply[Info[_], Repr[_], Arg](implicit
                                            forall: ForallRepr[Info, Repr],
                                            skir: SKIRepr[Info, Repr],
                                            arg: Info[Arg]) = new NextForall[Info, Repr, Arg] {
    override implicit def base: ForallRepr[Info, Repr] = forall

    override implicit def argi: Info[Arg] = arg

    override implicit def ski: SKIRepr[Info, Repr] = skir
  }
}