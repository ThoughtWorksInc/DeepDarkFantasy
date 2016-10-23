package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.CombUnit.{CombUnit, CombUnitExt}
import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Product.{ImpWProduct, ProductRepr}
import com.thoughtworks.DDF.Unit.{BEvalUnit, Unit}
import com.thoughtworks.DDF.{BEval, ImpW, Loss}

trait ImpWList[Info[_], Repr[_]] extends
  List[Lambda[X => (Info[X], Loss[X])], ImpW[Info, Repr, ?]] with
  ImpWProduct[Info, Repr] {
  override def listMap[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])):
  ImpW[Info, Repr, (A => B) => scala.List[A] => scala.List[B]] =
    ImpW(base.listMap(ai._1, bi._1), baseE.listMap(ai._2, bi._2))(rcun, becun)

  override def reverse[A](implicit ai: (Info[A], Loss[A])): ImpW[Info, Repr, scala.List[A] => scala.List[A]] =
    ImpW(base.reverse(ai._1), baseE.reverse(ai._2))(rcun, becun)

  override def foldRight[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])):
  ImpW[Info, Repr, (A => B => B) => B => scala.List[A] => B] =
    ImpW(base.foldRight(ai._1, bi._1), baseE.foldRight(ai._2, bi._2))(rcun, becun)

  override def foldLeft[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])):
  ImpW[Info, Repr, (A => B => A) => A => scala.List[B] => A] =
    ImpW(base.foldLeft(ai._1, bi._1), baseE.foldLeft(ai._2, bi._2))(rcun, becun)

  override def listZip[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])):
  ImpW[Info, Repr, scala.List[A] => scala.List[B] => scala.List[(A, B)]] =
    ImpW(base.listZip(ai._1, bi._1), baseE.listZip(ai._2, bi._2))(rcun, becun)

  override def scanLeft[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])):
  ImpW[Info, Repr, (B => A => B) => B => scala.List[A] => scala.List[B]] =
    ImpW(base.scanLeft(ai._1, bi._1), baseE.scanLeft(ai._2, bi._2))(rcun, becun)

  override def scanRight[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])):
  ImpW[Info, Repr, (A => B => B) => B => scala.List[A] => scala.List[B]] =
    ImpW(base.scanRight(ai._1, bi._1), baseE.scanRight(ai._2, bi._2))(rcun, becun)

  override implicit def listInfo[A](implicit ai: (Info[A], Loss[A])): (Info[scala.List[A]], Loss[scala.List[A]]) =
    (base.listInfo(ai._1), baseE.listInfo(ai._2))

  override def listElmInfo[A]: ((Info[scala.List[A]], Loss[scala.List[A]])) => (Info[A], Loss[A]) = i =>
    (base.listElmInfo(i._1), baseE.listElmInfo(i._2))

  override def rp: ProductRepr[Info, Repr] = base

  override def nil[A](implicit ai: (Info[A], Loss[A])): ImpW[Info, Repr, scala.List[A]] =
    ImpW(base.nil(ai._1), baseE.nil(ai._2))(rcun, becun)

  override def cons[A](implicit ai: (Info[A], Loss[A])): ImpW[Info, Repr, A => scala.List[A] => scala.List[A]] =
    ImpW(base.cons(ai._1), baseE.cons(ai._2))(rcun, becun)

  override def listMatch[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])):
  ImpW[Info, Repr, (scala.List[A]) => B => (A => scala.List[A] => B) => B] =
    ImpW(base.listMatch(ai._1, bi._1), baseE.listMatch(ai._2, bi._2))(rcun, becun)

  override def base: List[Info, Repr]

  override def baseE: List[Loss, BEval] = BEvalList.apply
}

object ImpWList {
  implicit def apply[Info[_], Repr[_]](implicit l: List[Info, Repr], u: Unit[Info, Repr], c: Comb[Info, Repr]):
  ImpWList[Info, Repr] = new ImpWList[Info, Repr] {
    override def runit: Unit[Info, Repr] = u

    override def base: List[Info, Repr] = l

    override def rcomb: Comb[Info, Repr] = c
  }
}
