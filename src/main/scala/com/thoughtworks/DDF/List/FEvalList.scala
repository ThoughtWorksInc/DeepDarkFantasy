package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.Language.LangTermLang
import com.thoughtworks.DDF.Product.FEvalProd
import com.thoughtworks.DDF.{FEval, FEvalCase, FEvalMatch}

trait FEvalList extends
  List[FEvalCase, FEval] with
  FEvalProd {
  override val base = LangTermLang

  override def scanLeft[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[(B => A => B) => B => scala.List[A] => scala.List[B]] {
      override val fec = aInfo(aInfo(bi, aInfo(ai, bi)), aInfo(bi, aInfo(listInfo(ai), listInfo(bi))))

      override def term[G: Gradient] = base.scanLeft(ai.wgi[G], bi.wgi[G])
    }

  override def listZip[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[scala.List[A] => scala.List[B] => scala.List[(A, B)]] {
      override val fec = aInfo(listInfo(ai), aInfo(listInfo(bi), listInfo(prodInfo(ai, bi))))

      override def term[G: Gradient] = base.listZip(ai.wgi[G], bi.wgi[G])
    }

  def lfem[A] = new FEvalMatch[scala.List[A]] {
    override type ret = FEvalCase[A]
  }

  override def listMap[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[(A => B) => scala.List[A] => scala.List[B]] {
      override val fec = aInfo(aInfo(ai, bi), aInfo(listInfo(ai), listInfo(bi)))

      override def term[G: Gradient] = base.listMap(ai.wgi[G], bi.wgi[G])
    }

  override def nil[A](implicit ai: FEvalCase[A]) =
    new FEval[scala.List[A]] {
      override val fec = listInfo(ai)

      override def term[G: Gradient] = base.nil(ai.wgi[G])
    }

  override def listMatch[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[scala.List[A] => B => (A => scala.List[A] => B) => B] {
      override val fec = aInfo(listInfo(ai), aInfo(bi, aInfo(aInfo(ai, aInfo(listInfo(ai), bi)), bi)))

      override def term[G: Gradient] = base.listMatch(ai.wgi[G], bi.wgi[G])
    }

  override implicit def listInfo[A](implicit ai: FEvalCase[A]):
  FEvalCase.Aux[scala.List[A], Lambda[G => scala.List[ai.WithGrad[G]]]] =
    new FEvalCase[scala.List[A]] {
      override type WithGrad[G] = scala.List[ai.WithGrad[G]]

      override def wgi[G: Gradient] = base.listInfo(ai.wgi[G])

      override val tm = lfem[A]

      override def tmr = ai
    }

  override def listElmInfo[A]: FEvalCase[scala.List[A]] => FEvalCase[A] = _.get(lfem[A])

  override def foldRight[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[(A => B => B) => B => scala.List[A] => B] {
      override val fec = aInfo(aInfo(ai, aInfo(bi, bi)), aInfo(bi, aInfo(listInfo(ai), bi)))

      override def term[G: Gradient] = base.foldRight(ai.wgi[G], bi.wgi[G])
    }

  override def foldLeft[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[(A => B => A) => A => scala.List[B] => A] {
      override val fec = aInfo(aInfo(ai, aInfo(bi, ai)), aInfo(ai, aInfo(listInfo(bi), ai)))

      override def term[G: Gradient] = base.foldLeft(ai.wgi[G], bi.wgi[G])
    }

  override def reverse[A](implicit ai: FEvalCase[A]) =
    new FEval[scala.List[A] => scala.List[A]] {
      override val fec = aInfo(listInfo(ai), listInfo(ai))

      override def term[G: Gradient] = base.reverse(ai.wgi[G])
    }

  override def scanRight[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[(A => B => B) => B => scala.List[A] => scala.List[B]] {
      override val fec = aInfo(aInfo(ai, aInfo(bi, bi)), aInfo(bi, aInfo(listInfo(ai), listInfo(bi))))

      override def term[G: Gradient] = base.scanRight(ai.wgi[G], bi.wgi[G])
    }

  override def cons[A](implicit ai: FEvalCase[A]) =
    new FEval[A => scala.List[A] => scala.List[A]] {
      override val fec = aInfo(ai, aInfo(listInfo(ai), listInfo(ai)))

      override def term[G: Gradient] = base.cons(ai.wgi[G])
    }
}

object FEvalList extends FEvalList