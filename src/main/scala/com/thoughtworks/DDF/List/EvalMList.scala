package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.NoInfo
import com.thoughtworks.DDF.Product.EvalMProd

trait EvalMList extends
  com.thoughtworks.DDF.List.List[NoInfo, Lambda[X => X]] with
  EvalMProd with
  SimpleList[Lambda[X => X]] {
  override def scanRight[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]):
  (A => B => B) => B => scala.List[A] => scala.List[B] =
    f => z => _.scanRight(z)((x, y) => f(x)(y))

  override def listMap[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]):
  (A => B) => scala.List[A] => scala.List[B] = f => _.map(f)

  override def scanLeft[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]):
  (B => A => B) => B => scala.List[A] => scala.List[B] =
    f => z => _.scanLeft(z)((x, y) => f(x)(y))

  override def listZip[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]):
  scala.List[A] => scala.List[B] => scala.List[(A, B)] = l => r =>
    l.zip(r)

  override def foldRight[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]):
  (A => B => B) => B => scala.List[A] => B = f => z =>
    _.foldRight(z)((x, y) => f(x)(y))

  override def foldLeft[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]):
  (A => B => A) => A => scala.List[B] => A = f => z =>
    _.foldLeft(z)((x, y) => f(x)(y))

  override def listMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]):
  scala.List[A] => B => (A => scala.List[A] => B) => B = {
    case Nil => x => _ => x
    case h :: t => _ => f => f(h)(t)
  }

  override def nil[A](implicit ai: NoInfo[A]): scala.List[A] = Nil

  override def cons[A](implicit ai: NoInfo[A]): A => scala.List[A] => scala.List[A] = h => t => h :: t

  override def reverse[A](implicit ai: NoInfo[A]): scala.List[A] => scala.List[A] = _.reverse
}

object EvalMList extends EvalMList