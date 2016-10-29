package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.ArrowLoss
import com.thoughtworks.DDF.Product.BEvalProduct
import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalList extends List[LossInfo, BEval] with BEvalListMin with BEvalProduct {
  override def listMap[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[(A => B) => scala.List[A] => scala.List[B]] =
    arrowEval[A => B, scala.List[A] => scala.List[B], ArrowLoss[A, bi.loss], ArrowLoss[scala.List[A], scala.List[bi.loss]]](ab =>
      (arrowEval[scala.List[A], scala.List[B], scala.List[ai.loss], scala.List[bi.loss]](la => {
        val lb = leval(la).map(x => aeval(ab).forward(x))
        (listEval(lb.map(_.eb)), l => lb.zip(l).map(x => x._1.backward(x._2)))
      }), _.mapReduce(la => l => leval(la).zip(l).map(p => ArrowLoss(p._1)(p._2)).
        foldRight(arrowInfo(ai, bi).m.zero)((x, y) => arrowInfo(ai, bi).m.append(x, y)))(arrowInfo(ai, bi).m)))

  override def reverse[A](implicit ai: LossInfo[A]): BEval[scala.List[A] => scala.List[A]] =
    arrowEval[scala.List[A], scala.List[A], scala.List[ai.loss], scala.List[ai.loss]](la => (listEval(leval(la).reverse), _.reverse))

  override def foldRight[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[(A => B => B) => B => scala.List[A] => B] =
    arrowEval[
      (A => B => B),
      B => scala.List[A] => B,
      ArrowLoss[A, ArrowLoss[B, bi.loss]],
      ArrowLoss[B, ArrowLoss[scala.List[A], bi.loss]]](abb =>
      (arrowEval[B, scala.List[A] => B, bi.loss, ArrowLoss[scala.List[A], bi.loss]](b =>
        (arrowEval[scala.List[A], B, scala.List[ai.loss], bi.loss](la => leval(la) match {
          case Nil => (b, _ => scala.List())
          case lh :: lt =>
            val blab = aeval(foldRight[A, B]).forward(abb)
            val lab = aeval(blab.eb).forward(b)
            val nb = aeval(lab.eb).forward(listEval(lt))
            val bb = aeval(abb).forward(lh)
            val finb = aeval(bb.eb).forward(nb.eb)
            (finb.eb, bl => {
              val rb = finb.backward(bl)
              val aih = bb.backward(ArrowLoss(nb.eb)(bl))
              aih :: nb.backward(rb)
            })
        })(listInfo(ai), bi), _.mapReduce(la => l => leval(la) match {
          case Nil => bi.m.zero
          case lh :: lt =>
            val blab = aeval(foldRight[A, B]).forward(abb)
            val lab = aeval(blab.eb).forward(b)
            val nb = aeval(lab.eb).forward(listEval(lt))
            val bb = aeval(abb).forward(lh)
            val finb = aeval(bb.eb).forward(nb.eb)
            lab.backward(ArrowLoss(listEval(lt))(finb.backward(l)))
        })(bi.m)))(bi, arrowInfo(listInfo(ai), bi)),
        _.mapReduce(b => _.mapReduce(la => l => leval(la) match {
          case Nil => arrowInfo(ai, arrowInfo(bi, bi)).m.zero
          case lh :: lt =>
            val blab = aeval(foldRight[A, B]).forward(abb)
            val lab = aeval(blab.eb).forward(b)
            val nb = aeval(lab.eb).forward(listEval(lt))
            val bb = aeval(abb).forward(lh)
            val finb = aeval(bb.eb).forward(nb.eb)
            arrowInfo(ai, arrowInfo(bi, bi)).m.append(
              ArrowLoss[A, ArrowLoss[B, bi.loss]](lh)(ArrowLoss[B, bi.loss](nb.eb)(l)),
              blab.backward(ArrowLoss(b)(ArrowLoss(listEval(lt))(finb.backward(l)))))
        })(arrowInfo(ai, arrowInfo(bi, bi)).m))(arrowInfo(ai, arrowInfo(bi, bi)).m)))

  override def foldLeft[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[(A => B => A) => A => scala.List[B] => A] =
    arrowEval[
      A => B => A,
      A => scala.List[B] => A,
      ArrowLoss[A, ArrowLoss[B, ai.loss]],
      ArrowLoss[A, ArrowLoss[scala.List[B], ai.loss]]](aba =>
      (arrowEval[A, scala.List[B] => A, ai.loss, ArrowLoss[scala.List[B], ai.loss]](a =>
        (arrowEval[scala.List[B], A, scala.List[bi.loss], ai.loss](lb => leval(lb) match {
          case Nil => (a, _ => List())
          case lh :: lt =>
            val ba = aeval(aba).forward(a)
            val na = aeval(ba.eb).forward(lh)
            val alba = aeval(foldLeft[A, B]).forward(aba)
            val lba = aeval(alba.eb).forward(na.eb)
            val res = aeval(lba.eb).forward(listEval(lt))
            (res.eb, ai => na.backward(lba.backward(ArrowLoss(listEval(lt))(ai))) +: res.backward(ai))
        })(listInfo(bi), ai),
          _.mapReduce(lb => l =>
            leval(lb) match {
              case Nil => l
              case lh :: lt =>
                val ba = aeval(aba).forward(a)
                val na = aeval(ba.eb).forward(lh)
                val alba = aeval(foldLeft[A, B]).forward(aba)
                val lba = aeval(alba.eb).forward(na.eb)
                ba.backward(ArrowLoss(lh)(lba.backward(ArrowLoss(listEval(lt))(l))))
            })(ai.m)))(ai, arrowInfo(listInfo(bi), ai)),
        _.mapReduce(a => _.mapReduce(lb => l => leval(lb) match {
          case Nil => arrowInfo(ai, arrowInfo(bi, ai)).m.zero
          case lh :: lt =>
            val ba = aeval(aba).forward(a)
            val na = aeval(ba.eb).forward(lh)
            val alba = aeval(foldLeft[A, B]).forward(aba)
            val lba = aeval(alba.eb).forward(na.eb)
            arrowInfo(ai, arrowInfo(bi, ai)).m.append(
              ArrowLoss(a)(ArrowLoss(lh)(lba.backward(ArrowLoss(listEval(lt))(l)))),
              alba.backward(ArrowLoss(na.eb)(ArrowLoss(listEval(lt))(l))))
        })(arrowInfo(ai, arrowInfo(bi, ai)).m))(arrowInfo(ai, arrowInfo(bi, ai)).m)))

  override def listZip[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[scala.List[A] => scala.List[B] => scala.List[(A, B)]] =
    arrowEval[
      scala.List[A],
      scala.List[B] => scala.List[(A, B)],
      scala.List[ai.loss],
      ArrowLoss[scala.List[B], scala.List[(ai.loss, bi.loss)]]](al =>
      (arrowEval[scala.List[B], scala.List[(A, B)], scala.List[bi.loss], scala.List[(ai.loss, bi.loss)]](bl =>
        (listEval(leval(al).zip(leval(bl)).map(p => productEval(p._1, p._2))), _.map(_._2))),
        _.mapReduce(_ => _.map(_._1))(listInfo(ai).m)))

  def scanLeftUC[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[((((B => A => B), B), scala.List[A])) => scala.List[B]] =
    arrowEval[
      ((((B => A => B), B), scala.List[A])),
      scala.List[B],
      ((ArrowLoss[B, ArrowLoss[A, bi.loss]], bi.loss), scala.List[ai.loss]),
      scala.List[bi.loss]](p => {
      val bab = peval(peval(p)._1)._1
      val b = peval(peval(p)._1)._2
      leval(peval(p)._2) match {
        case Nil => (listEval(List(b)), l => ((arrowInfo(bi, arrowInfo(ai, bi)).m.zero, l.head), listInfo(ai).m.zero))
        case lh :: lt =>
          val ab = aeval(bab).forward(b)
          val nb = aeval(ab.eb).forward(lh)
          val lb = aeval(scanLeftUC[A, B]).forward(productEval(productEval(bab, nb.eb), listEval(lt)))
          (listEval(b :: leval(lb.eb)), l => {
            val rl = lb.backward(l.tail)
            ((arrowInfo(bi, arrowInfo(ai, bi)).m.append(rl._1._1, ArrowLoss(b)(ArrowLoss(lh)(l.head))),
              bi.m.append(l.head, ab.backward(ArrowLoss(lh)(rl._1._2)))),
              nb.backward(rl._1._2) :: rl._2)
          })
      }
    })

  override def scanLeft[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[(B => A => B) => B => scala.List[A] => scala.List[B]] =
    app(curry[(B => A => B), B, scala.List[A] => scala.List[B]])(app(curry[((B => A => B), B), scala.List[A], scala.List[B]])(scanLeftUC[A, B]))

  def scanRightUC[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[((((A => B => B), B), scala.List[A])) => scala.List[B]] =
    arrowEval[
      ((((A => B => B), B), scala.List[A])),
      scala.List[B],
      ((ArrowLoss[A, ArrowLoss[B, bi.loss]], bi.loss), scala.List[ai.loss]),
      scala.List[bi.loss]](p => {
      val abb = peval(peval(p)._1)._1
      val b = peval(peval(p)._1)._2
      leval(peval(p)._2) match {
        case Nil => (listEval(List(b)), l => ((arrowInfo(ai, arrowInfo(bi, bi)).m.zero, l.head), listInfo(ai).m.zero))
        case lh :: lt =>
          val lb = aeval(scanRightUC[A, B]).forward(productEval(productEval(abb, b), listEval(lt)))
          val bb = aeval(abb).forward(lh)
          val nb = aeval(bb.eb).forward(leval(lb.eb).head)
          (listEval(nb.eb :: leval(lb.eb)), l => {
            val rl = lb.backward(l.tail)
            ((arrowInfo(ai, arrowInfo(bi, bi)).m.append(rl._1._1, ArrowLoss(lh)(ArrowLoss(leval(lb.eb).head)(l.head))),
              bi.m.append(rl._1._2, nb.backward(l.head))),
              bb.backward(ArrowLoss(leval(lb.eb).head)(l.head)) :: rl._2)
          })
      }
    })

  override def scanRight[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[(A => B => B) => B => scala.List[A] => scala.List[B]] =
    app(
      curry[(A => B => B), B, scala.List[A] => scala.List[B]])(
      app(curry[((A => B => B), B), scala.List[A], scala.List[B]])(scanRightUC[A, B]))
}

object BEvalList {
  implicit def apply = new BEvalList {}
}