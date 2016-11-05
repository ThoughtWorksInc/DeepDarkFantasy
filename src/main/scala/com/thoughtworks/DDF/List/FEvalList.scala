package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Language.LangTermLang
import com.thoughtworks.DDF.Product.FEvalProd
import com.thoughtworks.DDF.{FEMMatch, FEval, FEvalCase}

trait FEvalList[G] extends
  List[FEvalCase[G, ?], FEval[G, ?]] with
  FEvalProd[G] {
  override val base = LangTermLang

  override def scanLeft[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, (B => A => B) => B => scala.List[A] => scala.List[B]] {
      override val tm = aInfo(aInfo(bi, aInfo(ai, bi)), aInfo(bi, aInfo(listInfo(ai), listInfo(bi))))

      override val deriv = base.scanLeft(ai.lr, bi.lr)
    }

  override def listZip[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, scala.List[A] => scala.List[B] => scala.List[(A, B)]] {
      override val tm = aInfo(listInfo(ai), aInfo(listInfo(bi), listInfo(prodInfo(ai, bi))))

      override val deriv = base.listZip(ai.lr, bi.lr)
    }

  def lfem[A] = new FEMMatch[G, scala.List[A]] {
    override type ret = FEvalCase[G, A]
  }

  override def listMap[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, (A => B) => scala.List[A] => scala.List[B]] {
      override val tm = aInfo(aInfo(ai, bi), aInfo(listInfo(ai), listInfo(bi)))

      override val deriv = base.listMap(ai.lr, bi.lr)
    }

  override def nil[A](implicit ai: FEvalCase[G, A]) =
    new FEval[G, scala.List[A]] {
      override val tm = listInfo(ai)

      override val deriv = base.nil(ai.lr)
    }

  override def listMatch[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, scala.List[A] => B => (A => scala.List[A] => B) => B] {
      override val tm = aInfo(listInfo(ai), aInfo(bi, aInfo(aInfo(ai, aInfo(listInfo(ai), bi)), bi)))

      override val deriv = base.listMatch(ai.lr, bi.lr)
    }

  override implicit def listInfo[A](implicit ai: FEvalCase[G, A]):
  FEvalCase.Aux[G, scala.List[A], scala.List[ai.ret]] =
    new FEvalCase[G, scala.List[A]] {
      override type ret = scala.List[ai.ret]

      override def lr = base.listInfo(ai.lr)

      override val tm = lfem[A]

      override def tmr = ai
    }

  override def listElmInfo[A]: FEvalCase[G, scala.List[A]] => FEvalCase[G, A] = _.get(lfem[A])

  override def foldRight[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, (A => B => B) => B => scala.List[A] => B] {
      override val tm = aInfo(aInfo(ai, aInfo(bi, bi)), aInfo(bi, aInfo(listInfo(ai), bi)))

      override val deriv = base.foldRight(ai.lr, bi.lr)
    }

  override def foldLeft[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, (A => B => A) => A => scala.List[B] => A] {
      override val tm = aInfo(aInfo(ai, aInfo(bi, ai)), aInfo(ai, aInfo(listInfo(bi), ai)))

      override val deriv = base.foldLeft(ai.lr, bi.lr)
    }

  override def reverse[A](implicit ai: FEvalCase[G, A]) =
    new FEval[G, scala.List[A] => scala.List[A]] {
      override val tm = aInfo(listInfo(ai), listInfo(ai))

      override val deriv = base.reverse(ai.lr)
    }

  override def scanRight[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, (A => B => B) => B => scala.List[A] => scala.List[B]] {
      override val tm = aInfo(aInfo(ai, aInfo(bi, bi)), aInfo(bi, aInfo(listInfo(ai), listInfo(bi))))

      override val deriv = base.scanRight(ai.lr, bi.lr)
    }

  override def cons[A](implicit ai: FEvalCase[G, A]) =
    new FEval[G, A => scala.List[A] => scala.List[A]] {
      override val tm = aInfo(ai, aInfo(listInfo(ai), listInfo(ai)))

      override val deriv = base.cons(ai.lr)
    }
}

object FEvalList {
  implicit def apply[G]: FEvalList[G] = new FEvalList[G] { }
}