package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.BEvalArrowInfo
import com.thoughtworks.DDF.{BEval, BEvalCase, CommutativeMonoid, LossCase, LossInfo}

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

  override implicit def productInfo[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): LossInfo.Aux[(A, B), (ai.loss, bi.loss)] =
    new LossInfo[(A, B)] {
      override def convert: ((A, B)) => BEval[(A, B)] = p => productEval(ai.convert(p._1), bi.convert(p._2))

      override val lc: LossCase.Aux[(A, B), ProductLCRet[A, B]] = ProductLC[A, B]()

      override def lca: lc.ret = new ProductLCRet[A, B] {
        override def zeroth: LossInfo[A] = ai

        override def first: LossInfo[B] = bi
      }

      override type ret = (ai.loss, bi.loss)

      override def m: CommutativeMonoid[(ai.loss, bi.loss)] = new CommutativeMonoid[(ai.loss, bi.loss)] {

        override def zero: (ai.loss, bi.loss) = (ai.m.zero, bi.m.zero)

        override def append(f1: (ai.loss, bi.loss), f2: => (ai.loss, bi.loss)): (ai.loss, bi.loss) =
          (ai.m.append(f1._1, f2._1), bi.m.append(f1._2, f2._2))
      }

      override def update(x: (A, B))(rate: Double)(l: (ai.loss, bi.loss)): (A, B) =
        (ai.update(x._1)(rate)(l._1), bi.update(x._2)(rate)(l._2))
    }

  override def productZerothInfo[A, B]: LossInfo[(A, B)] => LossInfo[A] = p => witness(p.lc.unique(ProductLC[A, B]()))(p.lca).zeroth

  override def productFirstInfo[A, B]: LossInfo[(A, B)] => LossInfo[B] = p => witness(p.lc.unique(ProductLC[A, B]()))(p.lca).first
}

object BEvalProductInfo {
  implicit def apply: BEvalProductInfo = new BEvalProductInfo {}
}
