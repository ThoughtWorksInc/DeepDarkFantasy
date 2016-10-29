package com.thoughtworks.DDF.Language
import scalaz.NaturalTransformation

trait NextLang[Info[_], Repr[_], Arg] extends
  NTLang[
    Info,
    Repr,
    Lambda[X => Either[Repr[X], Repr[Arg => X]]]] {

  type repr[X] = Either[Repr[X], Repr[Arg => X]]

  implicit def argi: Info[Arg]

  override def reprInfo[A]: repr[A] => Info[A] = {
    case Left(x) => base.reprInfo(x)
    case Right(x) => base.rngInfo(base.reprInfo(x))
  }

  override def NTF = new NaturalTransformation[Repr, repr] {
    override def apply[A](fa: Repr[A]): Either[Repr[A], Repr[Arg => A]] = Left(fa)
  }

  override def app[A, B]: repr[A => B] => repr[A] => repr[B] = f => x => (f, x) match {
    case (Left(f_), Left(x_)) => Left(base.app(f_)(x_))
    case (Right(f_), Right(x_)) => Right(base.S__(f_)(x_))
    case (Left(f_), Right(_)) => app(Right(base.K_(f_)(argi)))(x)
    case (Right(_), Left(x_)) => app(f)(Right(base.K_(x_)(argi)))
  }

  val in: repr[Arg] = Right(base.I(argi))

  def collapse[A]: repr[A] => Repr[Arg => A] = {
    case Left(x) => base.K_(x)(argi)
    case Right(x) => x
  }

  def rconv[A]: Repr[A] => repr[A] = x => Left(x)
}

object NextLang {
  implicit def apply[Info[_], Repr[_], Arg](implicit l: Lang[Info, Repr], ai: Info[Arg]):
  NextLang[Info, Repr, Arg] =
    new NextLang[Info, Repr, Arg] {
      override def base: Lang[Info, Repr] = l

      override implicit def argi: Info[Arg] = ai
    }
}