package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Combinators.EvalMComb
import com.thoughtworks.DDF.NoInfo
import scalaz.effect.IO

trait EvalMInterLang extends InterLang[NoInfo, Lambda[X => X]] with SimpleLang[Lambda[X => X]] with EvalMComb {
  override def scanRight[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): (A => B => B) => B => List[A] => List[B] =
    f => z => _.scanRight(z)((x, y) => f(x)(y))

  override def divD: Double => Double => Double = l => r => l / r

  override def ltD: Double => Double => Boolean = l => r => l < r

  override def listMap[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): (A => B) => List[A] => List[B] = f => _.map(f)

  override def scanLeft[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): (B => A => B) => B => List[A] => List[B] =
    f => z => _.scanLeft(z)((x, y) => f(x)(y))

  override def listZip[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): List[A] => List[B] => List[(A, B)] = l => r =>
    l.zip(r)

  override def foldRight[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): (A => B => B) => B => List[A] => B = f => z =>
    _.foldRight(z)((x, y) => f(x)(y))

  override def foldLeft[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): (A => B => A) => A => List[B] => A = f => z =>
    _.foldLeft(z)((x, y) => f(x)(y))

  override def nil[A](implicit ai: NoInfo[A]): List[A] = Nil

  override def cons[A](implicit ai: NoInfo[A]): A => List[A] => List[A] = h => t => h :: t

  override def listMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): List[A] => B => (A => List[A] => B) => B = {
    case Nil => x => _ => x
    case h :: t => _ => f => f(h)(t)
  }

  override def none[A](implicit ai: NoInfo[A]): Option[A] = None

  override def some[A](implicit ai: NoInfo[A]): A => Option[A] = Some[A]

  override def optionMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): Option[A] => B => (A => B) => B = {
    case None => x => _ => x
    case Some(x) => _ => f => f(x)
  }

  override def uncurry[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]): (A => B => C) => ((A, B)) => C =
    f => x => f(x._1)(x._2)

  override def reverse[A](implicit ai: NoInfo[A]): List[A] => List[A] = _.reverse

  override def plusD: Double => Double => Double = l => r => l + r

  override def mkTop: Unit = Unit

  override def sigD: Double => Double = x => 1 / (1 + Math.exp(-x))

  override def mkProd[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): A => B => (A, B) = l => r => (l, r)

  override def zro[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): ((A, B)) => A = _._1

  override def fst[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): ((A, B)) => B = _._2

  override def multD: Double => Double => Double = l => r => l * r

  override def litD: Double => Double = identity[Double]

  override def curry[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]):
  (((A, B)) => C) => A => B => C = f => a => b => f((a, b))

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

  override def expD: Double => Double = Math.exp

  override def litB: Boolean => Boolean = identity[Boolean]

  override def ite[A](implicit ai: NoInfo[A]): Boolean => A => A => A = {
    case true => x => _ => x
    case false => _ => x => x
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