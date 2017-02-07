package com.thoughtworks.DDF

import scalaz.effect._

trait RecursiveInfoMatch[Self[X] <: RecursiveInfoMatch[Self, X], Y] extends TypeMatch[RecursiveInfoMatch[Self, Y]]

trait RecursiveInfo[Match[X] <: RecursiveInfoMatch[Match, X], Y] extends TypeCase[RecursiveInfoMatch[Match, Y]]

object RecursiveInfoMatch {
  type Aux[Match[X] <: RecursiveInfoMatch[Match, X], Y, Z] = RecursiveInfoMatch[Match, Y] { type ret = Z }

  trait BotRI[Match[X] <: RecursiveInfoMatch[Match, X]] extends RecursiveInfo[Match, Nothing] {
    override val tm = new RecursiveInfoMatch[Match, Nothing] {
      override type ret = Unit
    }

    override def tmr: tm.ret = ()
  }

  trait StringRI[Match[X] <: RecursiveInfoMatch[Match, X]] extends RecursiveInfo[Match, String] {
    override val tm = new RecursiveInfoMatch[Match, String] {
      override type ret = Unit
    }

    override def tmr: tm.ret = ()
  }

  trait BoolRI[Match[X] <: RecursiveInfoMatch[Match, X]] extends RecursiveInfo[Match, Boolean] {
    override val tm = new RecursiveInfoMatch[Match, Boolean] {
      override type ret = Unit
    }

    override def tmr: tm.ret = ()
  }

  trait DoubleRI[Match[X] <: RecursiveInfoMatch[Match, X]] extends RecursiveInfo[Match, Double] {
    override val tm = new RecursiveInfoMatch[Match, Double] {
      override type ret = Unit
    }

    override def tmr: tm.ret = ()
  }

  trait TopRI[Match[X] <: RecursiveInfoMatch[Match, X]] extends RecursiveInfo[Match, Unit] {
    override val tm = new RecursiveInfoMatch[Match, Unit] {
      override type ret = Unit
    }

    override def tmr: tm.ret = ()
  }

  def LM[Match[X] <: RecursiveInfoMatch[Match, X], Rec[X] <: RecursiveInfo[Match, X], A]: Aux[Match, List[A], Rec[A]] =
    new RecursiveInfoMatch[Match, List[A]] {
      override type ret = Rec[A]
    }

  trait ListRI[Match[X] <: RecursiveInfoMatch[Match, X], Rec[X] <: RecursiveInfo[Match, X], A]
    extends RecursiveInfo[Match, List[A]] {
    override val tm = LM[Match, Rec, A]

    override def tmr: Rec[A]
  }

  def IOM[Match[X] <: RecursiveInfoMatch[Match, X], Rec[X] <: RecursiveInfo[Match, X], A]: Aux[Match, IO[A], Rec[A]] =
    new RecursiveInfoMatch[Match, IO[A]] {
      override type ret = Rec[A]
    }

  trait IORI[Match[X] <: RecursiveInfoMatch[Match, X], Rec[X] <: RecursiveInfo[Match, X], A]
    extends RecursiveInfo[Match, IO[A]] {
    override val tm = IOM[Match, Rec, A]

    override def tmr: Rec[A]
  }

  def StM[Match[X] <: RecursiveInfoMatch[Match, X], Rec[X] <: RecursiveInfo[Match, X], A]:
  Aux[Match, Stream[A], Rec[A]] =
    new RecursiveInfoMatch[Match, Stream[A]] {
      override type ret = Rec[A]
    }

  trait StreamRI[Match[X] <: RecursiveInfoMatch[Match, X], Rec[X] <: RecursiveInfo[Match, X], A]
    extends RecursiveInfo[Match, Stream[A]] {
    override val tm = StM[Match, Rec, A]

    override def tmr: Rec[A]
  }

  def OM[Match[X] <: RecursiveInfoMatch[Match, X], Rec[X] <: RecursiveInfo[Match, X], A]:
  Aux[Match, Option[A], Rec[A]] =
    new RecursiveInfoMatch[Match, Option[A]] {
      override type ret = Rec[A]
    }

  trait OptionRI[Match[X] <: RecursiveInfoMatch[Match, X], Rec[X] <: RecursiveInfo[Match, X], A]
    extends RecursiveInfo[Match, Option[A]] {
    override val tm = OM[Match, Rec, A]

    override def tmr: Rec[A]
  }

  def AM[Match[X] <: RecursiveInfoMatch[Match, X], Rec[X] <: RecursiveInfo[Match, X], A, B]:
  Aux[Match, A => B, (Rec[A], Rec[B])] =
    new RecursiveInfoMatch[Match, A => B] {
      override type ret = (Rec[A], Rec[B])
    }

  trait ArrowRI[Match[X] <: RecursiveInfoMatch[Match, X], Rec[X] <: RecursiveInfo[Match, X], A, B]
    extends RecursiveInfo[Match, A => B] {
    override val tm = AM[Match, Rec, A, B]

    override def tmr: (Rec[A], Rec[B])
  }

  def PM[Match[X] <: RecursiveInfoMatch[Match, X], Rec[X] <: RecursiveInfo[Match, X], A, B]:
  Aux[Match, (A, B), (Rec[A], Rec[B])] =
    new RecursiveInfoMatch[Match, (A, B)] {
      override type ret = (Rec[A], Rec[B])
    }

  trait ProductRI[Match[X] <: RecursiveInfoMatch[Match, X], Rec[X] <: RecursiveInfo[Match, X], A, B]
    extends RecursiveInfo[Match, (A, B)] {
    override val tm = PM[Match, Rec, A, B]

    override def tmr: (Rec[A], Rec[B])
  }

  def SM[Match[X] <: RecursiveInfoMatch[Match, X], Rec[X] <: RecursiveInfo[Match, X], A, B]:
  Aux[Match, Either[A, B], (Rec[A], Rec[B])] =
    new RecursiveInfoMatch[Match, Either[A, B]] {
      override type ret = (Rec[A], Rec[B])
    }

  trait SumRI[Match[X] <: RecursiveInfoMatch[Match, X], Rec[X] <: RecursiveInfo[Match, X], A, B]
    extends RecursiveInfo[Match, Either[A, B]] {
    override val tm = SM[Match, Rec, A, B]

    override def tmr: (Rec[A], Rec[B])
  }
}