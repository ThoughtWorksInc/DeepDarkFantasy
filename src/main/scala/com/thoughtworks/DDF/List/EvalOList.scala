package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.EvalOArr
import com.thoughtworks.DDF.Combinators.EvalOComb
import com.thoughtworks.DDF.Language.InterLangTerm
import com.thoughtworks.DDF.Product.EvalOProdMin
import com.thoughtworks.DDF.{EvalO, EvalOMatch}

trait EvalOList extends
  com.thoughtworks.DDF.List.List[InterLangInfoG, EvalO] with
  ILIGListInfo[EvalO] with
  EvalOArr with
  EvalOComb with
  EvalOProdMin {
  override def scanLeft[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]):
  EvalO[(B => A => B) => B => scala.List[A] => scala.List[B]] =
    aeval(ltl.scanLeft[A, B])(f => aeval(ltl.scanLeft_(f.l))(z => aeval(ltl.scanLeft__(f.l)(z.l))(l =>
      leval(getList(l).scanLeft(z)((x, y) => app(app(f)(x))(y))))))

  override def listZip[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]):
  EvalO[scala.List[A] => scala.List[B] => scala.List[(A, B)]] =
    aeval(ltl.listZip[A, B])(l => aeval(ltl.listZip_[A, B](l.l))(r =>
      leval(getList(l).zip(getList(r)).map(p => mkProd__(p._1)(p._2)))))

  override def foldRight[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]):
  EvalO[(A => B => B) => B => scala.List[A] => B] =
    aeval(ltl.foldRight[A, B])(f => aeval(ltl.foldRight_(f.l))(z => aeval(ltl.foldRight__(f.l)(z.l))(l =>
      getList(l).foldRight(z)((x, y) => app(app(f)(x))(y)))))

  override def scanRight[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]):
  EvalO[(A => B => B) => B => scala.List[A] => scala.List[B]] =
    aeval(ltl.scanRight[A, B])(f => aeval(ltl.scanRight_(f.l))(z => aeval(ltl.scanRight__(f.l)(z.l))(l =>
      leval(getList(l).scanRight(z)((x, y) => app(app(f)(x))(y))))))

  override def foldLeft[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]):
  EvalO[(A => B => A) => A => scala.List[B] => A] =
    aeval(ltl.foldLeft[A, B])(f => aeval(ltl.foldLeft_(f.l))(z => aeval(ltl.foldLeft__(f.l)(z.l))(l =>
      getList(l).foldLeft(z)((x, y) => app(app(f)(x))(y)))))

  override def listMap[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]):
  EvalO[(A => B) => scala.List[A] => scala.List[B]] =
    aeval(ltl.listMap[A, B])(f => aeval(ltl.listMap_(f.l))(l => leval(getList(l).map(app(f)))))

  override def reverse[A](implicit ai: InterLangInfoG[A]): EvalO[scala.List[A] => scala.List[A]] =
    aeval(ltl.reverse[A])(l => leval(getList(l).reverse))

  def ltm[A]: EvalOMatch.Aux[scala.List[A], Option[(EvalO[A], EvalO[scala.List[A]])]] = new EvalOMatch[scala.List[A]] {
    override type ret = Option[(EvalO[A], EvalO[scala.List[A]])]
  }

  def getList[A]: EvalO[scala.List[A]] => scala.List[EvalO[A]] = _.get(ltm[A]) match {
    case None => Nil
    case Some((h, t)) => h :: getList(t)
  }

  def leval[A](l: scala.List[EvalO[A]])(implicit ai: InterLangInfoG[A]): EvalO[scala.List[A]] = l match {
    case Nil => nil[A]
    case h :: t => cons__(h)(leval(t))
  }

  override def nil[A](implicit ai: InterLangInfoG[A]): EvalO[scala.List[A]] = new EvalO[scala.List[A]] {
    override def l: InterLangTerm[scala.List[A]] = ltl.nil[A]

    override def tmr: tm.ret = None

    override val tm = ltm[A]
  }

  override def cons[A](implicit ai: InterLangInfoG[A]): EvalO[A => scala.List[A] => scala.List[A]] =
    aeval(ltl.cons[A])(h => aeval(ltl.cons_(h.l))(t => new EvalO[scala.List[A]] {
      override def l: InterLangTerm[scala.List[A]] = ltl.cons__(h.l)(t.l)

      override def tmr: tm.ret = Some((h, t))

      override val tm = ltm[A]
    }))

  override def listMatch[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]):
  EvalO[scala.List[A] => B => (A => scala.List[A] => B) => B] = aeval(ltl.listMatch[A, B])(_.get(ltm[A]) match {
    case None => K[B, (A => scala.List[A] => B)]
    case Some((h, t)) => K_(B__(Let_[scala.List[A], B](t))(Let_[A, scala.List[A] => B](h)))
  })
}

object EvalOList extends EvalOList