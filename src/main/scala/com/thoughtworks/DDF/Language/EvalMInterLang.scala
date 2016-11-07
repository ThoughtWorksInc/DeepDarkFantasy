package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Combinators.EvalMComb
import com.thoughtworks.DDF.Double.EvalMDouble
import com.thoughtworks.DDF.List.EvalMList
import com.thoughtworks.DDF.NoInfo

import scalaz.effect.IO

trait EvalMInterLang extends
  InterLang[NoInfo, Lambda[X => X]] with
  SimpleLang[Lambda[X => X]] with
  EvalMComb with
  EvalMList with
  EvalMDouble {
  override def none[A](implicit ai: NoInfo[A]): Option[A] = None

  override def some[A](implicit ai: NoInfo[A]): A => Option[A] = Some[A]

  override def optionMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): Option[A] => B => (A => B) => B = {
    case None => x => _ => x
    case Some(x) => _ => f => f(x)
  }

  override def mkTop: Unit = Unit

  override def sumComm[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): Either[A, B] => Either[B, A] = _.swap

  override def sumAssocLR[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]):
  Either[Either[A, B], C] => Either[A, Either[B, C]] = {
    case Left(Left(x)) => Left(x)
    case Left(Right(x)) => Right(Left(x))
    case Right(x) => Right(Right(x))
  }

  override def sumAssocRL[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]):
  Either[A, Either[B, C]] => Either[Either[A, B], C] = {
    case Left(x) => Left(Left(x))
    case Right(Left(x)) => Left(Right(x))
    case Right(Right(x)) => Right(x)
  }

  override def left[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): A => Either[A, B] = Left[A, B]

  override def right[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): B => Either[A, B] = Right[A, B]

  override def sumMatch[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]):
  Either[A, B] => (A => C) => (B => C) => C = {
    case Left(x) => f => _ => f(x)
    case Right(x) => _ => f => f(x)
  }

  override def exfalso[A](implicit ai: NoInfo[A]): Nothing => A = x => x

  override def putDouble: Double => IO[Unit] = d => IO.put[Double](d)(scalaz.std.anyVal.doubleInstance)

  override def IOBind[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): IO[A] => (A => IO[B]) => IO[B] = _.flatMap

  override def IORet[A](implicit ai: NoInfo[A]): A => IO[A] = a => IO(a)

  override def getDouble: IO[Double] = IO.readLn.map(_.toDouble)

  override def IOInfo[A](implicit ai: NoInfo[A]): NoInfo[IO[A]] = NoInfo()

  override def IOElmInfo[A]: NoInfo[IO[A]] => NoInfo[A] = _ => NoInfo()

  override def streamNil[A](implicit ai: NoInfo[A]): Stream[A] = Stream.empty

  override def streamCons[A](implicit ai: NoInfo[A]) = a => ls => Stream.cons(a, ls(() : Unit))

  override def streamMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = {
    case Stream.Empty => x => _ => x
    case h #:: t => _ => f => f(h)(t)
  }
}

object EvalMInterLang extends EvalMInterLang