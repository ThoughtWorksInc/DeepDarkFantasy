package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.BEvalArrowInfo
import com.thoughtworks.DDF.{BEval, BEvalMatch, CommutativeMonoid, Loss, LossMatch, LossInfo}


trait BEvalProductInfo extends ProductInfo[LossInfo, BEval] with BEvalArrowInfo {
  trait ProductLCRet[A, B] {
    def zeroth: LossInfo[A]

    def first: LossInfo[B]
  }

  case class ProductBEC[A, B]() extends BEvalMatch[(A, B)] {
    override type ret = (BEval[A], BEval[B])
  }

  case class ProductLC[A, B]() extends LossMatch[(A, B)] {
    override type ret = ProductLCRet[A, B]
  }

  def peval[A, B](ab: BEval[(A, B)]): (BEval[A], BEval[B]) = ab.get(ProductBEC[A, B]())

  def productEval[A, B](a: BEval[A], b: BEval[B])(implicit al: LossInfo[A], bl: LossInfo[B]) = new BEval[(A, B)] {
    override val loss: LossInfo[(A, B)] = productInfo(al, bl)

    override def eval: (A, B) = (a.eval, b.eval)

    override val tm: BEvalMatch.Aux[(A, B), (BEval[A], BEval[B])] = ProductBEC()

    override val tmr: tm.ret = (a, b)
  }

  def lossP[A, B]: Loss[A] => Loss[B] => Loss[(A, B)] = l => r => new Loss[(A, B)] {
    override val tm: LossInfo.Aux[(A, B), (Loss[A], Loss[B])] = productInfo(l.tm, r.tm)

    override val tmr: tm.loss = (l, r)
  }

  def ploss[A, B]: Loss[(A, B)] => (Loss[A], Loss[B]) = l =>
    l.get(productInfo(productZerothInfo(l.tm), productFirstInfo(l.tm)))

  def ploss0[A, B]: Loss[(A, B)] => Loss[A] = l => ploss(l)._1

  def ploss1[A, B]: Loss[(A, B)] => Loss[B] = l => ploss(l)._2

  override implicit def productInfo[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]):
  LossInfo.Aux[(A, B), (Loss[A], Loss[B])] =
    new LossInfo[(A, B)] {
      override def convert: ((A, B)) => BEval[(A, B)] = p => productEval(ai.convert(p._1), bi.convert(p._2))

      override val tm: LossMatch.Aux[(A, B), ProductLCRet[A, B]] = ProductLC[A, B]()

      override val tmr: tm.ret = new ProductLCRet[A, B] {
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

  override def productZerothInfo[A, B]: LossInfo[(A, B)] => LossInfo[A] = _.get(ProductLC[A, B]()).zeroth

  override def productFirstInfo[A, B]: LossInfo[(A, B)] => LossInfo[B] = _.get(ProductLC[A, B]()).first
}

object BEvalProductInfo {
  implicit def apply: BEvalProductInfo = new BEvalProductInfo {}
}
