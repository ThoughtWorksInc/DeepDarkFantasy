package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.Language.LangTermLang
import com.thoughtworks.DDF.Product.ADEvalProd
import com.thoughtworks.DDF.{ADEval, ADEvalCase, ADEvalMatch}

trait ADEvalList extends
  List[ADEvalCase, ADEval] with
  ADEvalProd {
  override val base = LangTermLang

  override def scanLeft[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[(B => A => B) => B => scala.List[A] => scala.List[B]] {
      override val fec = aInfo(aInfo(bi, aInfo(ai, bi)), aInfo(bi, aInfo(listInfo(ai), listInfo(bi))))

      override def term[G: Gradient] = base.scanLeft(ai.wgi[G], bi.wgi[G])
    }

  override def listZip[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[scala.List[A] => scala.List[B] => scala.List[(A, B)]] {
      override val fec = aInfo(listInfo(ai), aInfo(listInfo(bi), listInfo(prodInfo(ai, bi))))

      override def term[G: Gradient] = base.listZip(ai.wgi[G], bi.wgi[G])
    }

  def lfem[A] = new ADEvalMatch[scala.List[A]] {
    override type ret = ADEvalCase[A]
  }

  override def listMap[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[(A => B) => scala.List[A] => scala.List[B]] {
      override val fec = aInfo(aInfo(ai, bi), aInfo(listInfo(ai), listInfo(bi)))

      override def term[G: Gradient] = base.listMap(ai.wgi[G], bi.wgi[G])
    }

  override def nil[A](implicit ai: ADEvalCase[A]) =
    new ADEval[scala.List[A]] {
      override val fec = listInfo(ai)

      override def term[G: Gradient] = base.nil(ai.wgi[G])
    }

  override def listMatch[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[scala.List[A] => B => (A => scala.List[A] => B) => B] {
      override val fec = aInfo(listInfo(ai), aInfo(bi, aInfo(aInfo(ai, aInfo(listInfo(ai), bi)), bi)))

      override def term[G: Gradient] = base.listMatch(ai.wgi[G], bi.wgi[G])
    }

  override implicit def listInfo[A](implicit ai: ADEvalCase[A]):
  ADEvalCase.Aux[scala.List[A], Lambda[G => scala.List[ai.WithGrad[G]]]] =
    new ADEvalCase[scala.List[A]] {
      override type WithGrad[G] = scala.List[ai.WithGrad[G]]

      override def wgi[G: Gradient] = base.listInfo(ai.wgi[G])

      override val tm = lfem[A]

      override def tmr = ai
    }

  override def listElmInfo[A]: ADEvalCase[scala.List[A]] => ADEvalCase[A] = _.get(lfem[A])

  override def foldRight[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[(A => B => B) => B => scala.List[A] => B] {
      override val fec = aInfo(aInfo(ai, aInfo(bi, bi)), aInfo(bi, aInfo(listInfo(ai), bi)))

      override def term[G: Gradient] = base.foldRight(ai.wgi[G], bi.wgi[G])
    }

  override def foldLeft[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[(A => B => A) => A => scala.List[B] => A] {
      override val fec = aInfo(aInfo(ai, aInfo(bi, ai)), aInfo(ai, aInfo(listInfo(bi), ai)))

      override def term[G: Gradient] = base.foldLeft(ai.wgi[G], bi.wgi[G])
    }

  override def reverse[A](implicit ai: ADEvalCase[A]) =
    new ADEval[scala.List[A] => scala.List[A]] {
      override val fec = aInfo(listInfo(ai), listInfo(ai))

      override def term[G: Gradient] = base.reverse(ai.wgi[G])
    }

  override def scanRight[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[(A => B => B) => B => scala.List[A] => scala.List[B]] {
      override val fec = aInfo(aInfo(ai, aInfo(bi, bi)), aInfo(bi, aInfo(listInfo(ai), listInfo(bi))))

      override def term[G: Gradient] = base.scanRight(ai.wgi[G], bi.wgi[G])
    }

  override def cons[A](implicit ai: ADEvalCase[A]) =
    new ADEval[A => scala.List[A] => scala.List[A]] {
      override val fec = aInfo(ai, aInfo(listInfo(ai), listInfo(ai)))

      override def term[G: Gradient] = base.cons(ai.wgi[G])
    }
}

object ADEvalList extends ADEvalList