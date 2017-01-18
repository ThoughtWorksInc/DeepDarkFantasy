package com.thoughtworks.DDF.Language
import com.thoughtworks.DDF.Combinators.Comb

import scalaz.NaturalTransformation

trait NextInterLang[Info[_], Repr[_], Arg] extends
  InterLang[Info, Lambda[X => (Info[X], Either[Repr[X], Repr[Arg => X]])]] with
  NTInterLang[
    Info,
    Repr,
    Lambda[X => (Info[X], Either[Repr[X], Repr[Arg => X]])]] with
  NextBase[Info, Repr, Arg] {
  override implicit def comb = base

  override def NTF = new NaturalTransformation[Repr, repr] {
    override def apply[A](fa: Repr[A]): Either[Repr[A], Repr[Arg => A]] = Left(fa)
  }

  override def app[A, B]: repr[A => B] => repr[A] => repr[B] = f => x => (f._2, x._2) match {
    case (Left(f_), Left(x_)) => Left(base.app(f_)(x_))
    case (Right(f_), Right(x_)) => Right(base.S__(f_)(x_))
    case (Left(f_), Right(x_)) => Right(base.B__(f_)(x_))
    case (Right(f_), Left(x_)) => Right(base.C__(f_)(x_))
  }
}

object NextInterLang {
  implicit def apply[Info[_], Repr[_], Arg](implicit l: InterLang[Info, Repr], ai: Info[Arg]):
  NextInterLang[Info, Repr, Arg] =
    new NextInterLang[Info, Repr, Arg] {
      override def base: InterLang[Info, Repr] = l

      override implicit def argi: Info[Arg] = ai
    }
}