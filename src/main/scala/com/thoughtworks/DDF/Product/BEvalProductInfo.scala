package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.BEvalArrowInfo
import com.thoughtworks.DDF.{BEval, BEvalCase, CommutativeMonoid, Loss, LossCase, LossInfo}

import scalaz.Leibniz.witness

trait BEvalProductInfo extends ProductInfo[LossInfo, BEval] with BEvalArrowInfo {
  trait ProductLCRet[A, B] {
    def zeroth: LossInfo[A]

    def first: LossInfo[B]
  }

  case class ProductBEC[A, B]() extends BEvalCase[(A, B)] {
    override type ret = (BEval[A], BEval[B])
  }

  case class ProductLC[A, B]() extends LossCase[(A, B)] {
    override type ret = ProductLCRet[A, B]
  }

  def peval[A, B](ab: BEval[(A, B)]): (BEval[A], BEval[B]) = witness(ab.ec.unique(ProductBEC[A, B]()))(ab.eca)

  def productEval[A, B](a: BEval[A], b: BEval[B])(implicit al: LossInfo[A], bl: LossInfo[B]) = new BEval[(A, B)] {
    override val loss: LossInfo[(A, B)] = productInfo(al, bl)

    override def eval: (A, B) = (a.eval, b.eval)

    override val ec: BEvalCase.Aux[(A, B), (BEval[A], BEval[B])] = ProductBEC()

    override def eca: ec.ret = (a, b)
  }

  def lossP[A, B]: Loss[A] => Loss[B] => Loss[(A, B)] = l => r => new Loss[(A, B)] {
    override val li: LossInfo.Aux[(A, B), (Loss[A], Loss[B])] = productInfo(l.li, r.li)

    override val x: li.loss = (l, r)
  }

  def ploss[A, B]: Loss[(A, B)] => (Loss[A], Loss[B]) = l =>
    witness(l.li.unique(productInfo(productZerothInfo(l.li), productFirstInfo(l.li))))(l.x)

  def ploss0[A, B]: Loss[(A, B)] => Loss[A] = l => ploss(l)._1

  def ploss1[A, B]: Loss[(A, B)] => Loss[B] = l => ploss(l)._2

  override implicit def productInfo[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]):
  LossInfo.Aux[(A, B), (Loss[A], Loss[B])] =
    new LossInfo[(A, B)] {
      override def convert: ((A, B)) => BEval[(A, B)] = p => productEval(ai.convert(p._1), bi.convert(p._2))

      override val lc: LossCase.Aux[(A, B), ProductLCRet[A, B]] = ProductLC[A, B]()

      override def lca: lc.ret = new ProductLCRet[A, B] {
        override def zeroth: LossInfo[A] = ai

        override def first: LossInfo[B] = bi
      }

      override type ret = (Loss[A], Loss[B])

      override def m: CommutativeMonoid[(Loss[A], Loss[B])] = new CommutativeMonoid[(Loss[A], Loss[B])] {

        override def zero: (Loss[A], Loss[B]) = (ai.lm.zero, bi.lm.zero)

        override def append(f1: (Loss[A], Loss[B]), f2: => (Loss[A], Loss[B])): (Loss[A], Loss[B]) =
          (ai.lm.append(f1._1, f2._1), bi.lm.append(f1._2, f2._2))
      }

      override def update(x: (A, B))(rate: Double)(l: (Loss[A], Loss[B])): (A, B) =
        (ai.updatel(x._1)(rate)(l._1), bi.updatel(x._2)(rate)(l._2))
    }

  override def productZerothInfo[A, B]: LossInfo[(A, B)] => LossInfo[A] = p =>
    witness(p.lc.unique(ProductLC[A, B]()))(p.lca).zeroth

  override def productFirstInfo[A, B]: LossInfo[(A, B)] => LossInfo[B] = p => witness(p.lc.unique(ProductLC[A, B]()))(p.lca).first
}

object BEvalProductInfo {
  implicit def apply: BEvalProductInfo = new BEvalProductInfo {}
}
