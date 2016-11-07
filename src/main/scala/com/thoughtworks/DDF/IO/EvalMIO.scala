package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.Double.EvalMDouble
import com.thoughtworks.DDF.NoInfo
import com.thoughtworks.DDF.Top.EvalMTop

import scalaz.effect

trait EvalMIO extends
  IO[NoInfo, Lambda[X => X]] with
  SimpleIO[Lambda[X => X]] with
  EvalMTop with
  EvalMDouble {
  override def putDouble: Double => IO[Unit] = d => effect.IO.put[Double](d)(scalaz.std.anyVal.doubleInstance)

  override def IOBind[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]):
  effect.IO[A] => (A => effect.IO[B]) => effect.IO[B] = _.flatMap

  override def IORet[A](implicit ai: NoInfo[A]): A => effect.IO[A] = a => effect.IO(a)

  override def getDouble: effect.IO[Double] = effect.IO.readLn.map(_.toDouble)
}

object EvalMIO extends EvalMIO