package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.NoInfo
import com.thoughtworks.DDF.Product.EvalMProd

trait EvalMSum extends
  Sum[NoInfo, Lambda[X => X]] with
  SimpleSum[Lambda[X => X]] with
  EvalMProd {
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
}

object EvalMSum extends EvalMSum