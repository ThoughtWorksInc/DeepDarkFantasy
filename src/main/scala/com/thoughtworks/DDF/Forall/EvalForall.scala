package com.thoughtworks.DDF.Forall

import com.thoughtworks.DDF.Arrow.EvalArrow
import com.thoughtworks.DDF.{Eval, EvalCase, Loss, LossCase}

import scalaz.{Forall, Monoid, NaturalTransformation}
import scalaz.Leibniz._

trait EvalForall extends ForallRepr[Loss, Eval] with EvalArrow {
  override def forallInfo[F[_]](implicit fi: this.FInfo[F]): Loss[Forall[F]] =
    new Loss[Forall[F]] {
      override def m: Monoid[Unit] = new Monoid[Unit] {
        override def zero: Unit = ()

        override def append(f1: Unit, f2: => Unit): Unit = ()
      }

      override def convert: Forall[F] => Eval[Forall[F]] = ???

      override val lc: LossCase.Aux[Forall[F], Unit] = new LossCase[Forall[F]] {
        override type ret = Unit
      }

      override def lca: lc.ret = ()

      override type ret = Unit
    }

  case class ForallEC[F[_]]() extends EvalCase[Forall[F]] {
    override type ret = NaturalTransformation[Loss, Lambda[X => Eval[F[X]]]]
  }

  def ForallEval[F[_]](f: Forall[F]) = new Eval[Forall[F]] {
    override val loss: Loss[Forall[F]] = ???

    override def eval: Forall[F] = ???

    override val ec: EvalCase[Forall[F]] = ???

    override def eca: ec.ret = ???
  }

  def feval[F[_], A](f: Eval[Forall[F]])(implicit ai: Loss[A]): Eval[F[A]] =
    witness(f.ec.unique(ForallEC[F]()))(f.eca).apply(ai)

  override def specialize[F[_], A](implicit ai: Loss[A], fi: FInfo[F]): Eval[Forall[F] => F[A]] = ???
}

object EvalForall {
  implicit def apply = new EvalForall {}
}