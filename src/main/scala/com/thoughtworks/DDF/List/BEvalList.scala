package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Product.BEvalProd
import com.thoughtworks.DDF.{BEval, Loss, LossInfo}

trait BEvalList extends List[LossInfo, BEval] with BEvalListMin with BEvalProd {
  override def listMap[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]):
  BEval[(A => B) => scala.List[A] => scala.List[B]] =
    aEval[A => B, scala.List[A] => scala.List[B]](ab =>
      (aEval[scala.List[A], scala.List[B]](la => {
        val lb = leval(la).map(x => aeval(ab).forward(x))
        (listEval(lb.map(_.eb)), l => lLoss(lb.zip(lloss(l)).map(x => x._1.backward(x._2))))
      }), x => aloss(x).mapReduce(la => l => leval(la).zip(lloss(l)).map(p => lossA(p._1)(p._2)).
        foldRight(aInfo(ai, bi).lm.zero)((x, y) => aInfo(ai, bi).lm.append(x, y)))(aInfo(ai, bi).lm)))

  override def reverse[A](implicit ai: LossInfo[A]): BEval[scala.List[A] => scala.List[A]] =
    aEval[scala.List[A], scala.List[A]](la => (listEval(leval(la).reverse), l => lLoss(lloss(l).reverse)))

  override def foldRight[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[(A => B => B) => B => scala.List[A] => B] =
    aEval[(A => B => B), B => scala.List[A] => B](abb =>
      (aEval[B, scala.List[A] => B](b =>
        (aEval[scala.List[A], B](la => leval(la) match {
          case Nil => (b, _ => lLoss(scala.List[Loss[A]]()))
          case lh :: lt =>
            val blab = aeval(foldRight[A, B]).forward(abb)
            val lab = aeval(blab.eb).forward(b)
            val nb = aeval(lab.eb).forward(listEval(lt))
            val bb = aeval(abb).forward(lh)
            val finb = aeval(bb.eb).forward(nb.eb)
            (finb.eb, bl => {
              val rb = finb.backward(bl)
              val aih = bb.backward(lossA(nb.eb)(bl))
              lLoss(aih :: lloss(nb.backward(rb)))
            })
        })(listInfo(ai), bi), x => aloss(x).mapReduce(la => l => leval(la) match {
          case Nil => bi.lm.zero
          case lh :: lt =>
            val blab = aeval(foldRight[A, B]).forward(abb)
            val lab = aeval(blab.eb).forward(b)
            val nb = aeval(lab.eb).forward(listEval(lt))
            val bb = aeval(abb).forward(lh)
            val finb = aeval(bb.eb).forward(nb.eb)
            lab.backward(lossA(listEval(lt))(finb.backward(l)))
        })(bi.lm)))(bi, aInfo(listInfo(ai), bi)),
        x => aloss(x).mapReduce(b => y => aloss(y).mapReduce(la => l => leval(la) match {
          case Nil => aInfo(ai, aInfo(bi, bi)).lm.zero
          case lh :: lt =>
            val blab = aeval(foldRight[A, B]).forward(abb)
            val lab = aeval(blab.eb).forward(b)
            val nb = aeval(lab.eb).forward(listEval(lt))
            val bb = aeval(abb).forward(lh)
            val finb = aeval(bb.eb).forward(nb.eb)
            aInfo(ai, aInfo(bi, bi)).lm.append(
              lossA[A, B => B](lh)(lossA[B, B](nb.eb)(l)),
              blab.backward(lossA(b)(lossA(listEval(lt))(finb.backward(l)))))
        })(aInfo(ai, aInfo(bi, bi)).lm))(aInfo(ai, aInfo(bi, bi)).lm)))

  override def foldLeft[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[(A => B => A) => A => scala.List[B] => A] =
    aEval[A => B => A, A => scala.List[B] => A](aba =>
      (aEval[A, scala.List[B] => A](a =>
        (aEval[scala.List[B], A](lb => leval(lb) match {
          case Nil => (a, _ => lLoss(List[Loss[B]]()))
          case lh :: lt =>
            val ba = aeval(aba).forward(a)
            val na = aeval(ba.eb).forward(lh)
            val alba = aeval(foldLeft[A, B]).forward(aba)
            val lba = aeval(alba.eb).forward(na.eb)
            val res = aeval(lba.eb).forward(listEval(lt))
            (res.eb, ai => lLoss(na.backward(lba.backward(lossA(listEval(lt))(ai))) +: lloss(res.backward(ai))))
        })(listInfo(bi), ai),
          x => aloss(x).mapReduce(lb => l =>
            leval(lb) match {
              case Nil => l
              case lh :: lt =>
                val ba = aeval(aba).forward(a)
                val na = aeval(ba.eb).forward(lh)
                val alba = aeval(foldLeft[A, B]).forward(aba)
                val lba = aeval(alba.eb).forward(na.eb)
                ba.backward(lossA(lh)(lba.backward(lossA(listEval(lt))(l))))
            })(ai.lm)))(ai, aInfo(listInfo(bi), ai)),
        x => aloss(x).mapReduce(a => y => aloss(y).mapReduce(lb => l => leval(lb) match {
          case Nil => aInfo(ai, aInfo(bi, ai)).lm.zero
          case lh :: lt =>
            val ba = aeval(aba).forward(a)
            val na = aeval(ba.eb).forward(lh)
            val alba = aeval(foldLeft[A, B]).forward(aba)
            val lba = aeval(alba.eb).forward(na.eb)
            aInfo(ai, aInfo(bi, ai)).lm.append(
              lossA(a)(lossA(lh)(lba.backward(lossA(listEval(lt))(l)))),
              alba.backward(lossA(na.eb)(lossA(listEval(lt))(l))))
        })(aInfo(ai, aInfo(bi, ai)).lm))(aInfo(ai, aInfo(bi, ai)).lm)))

  override def listZip[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]):
  BEval[scala.List[A] => scala.List[B] => scala.List[(A, B)]] =
    aEval[scala.List[A], scala.List[B] => scala.List[(A, B)]](al =>
      (aEval[scala.List[B], scala.List[(A, B)]](bl =>
        (listEval(leval(al).zip(leval(bl)).map(p => productEval(p._1, p._2))), l => lLoss(lloss(l).map(ploss1)))),
        x => aloss(x).mapReduce(_ => l => lLoss(lloss(l).map(ploss0)))(listInfo(ai).lm)))

  def scanLeftUC[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]):
  BEval[((((B => A => B), B), scala.List[A])) => scala.List[B]] =
    aEval[(((B => A => B), B), scala.List[A]), scala.List[B]](p => {
      val bab = peval(peval(p)._1)._1
      val b = peval(peval(p)._1)._2
      leval(peval(p)._2) match {
        case Nil =>
          (listEval(List(b)), l => lossP(lossP(aInfo(bi, aInfo(ai, bi)).lm.zero)(lloss(l).head))(listInfo(ai).lm.zero))
        case lh :: lt =>
          val ab = aeval(bab).forward(b)
          val nb = aeval(ab.eb).forward(lh)
          val lb = aeval(scanLeftUC[A, B]).forward(productEval(productEval(bab, nb.eb), listEval(lt)))
          (listEval(b :: leval(lb.eb)), l => {
            val rl = lb.backward(lLoss(lloss(l).tail))
            lossP(
              lossP(
                aInfo(bi, aInfo(ai, bi)).lm.append(ploss0(ploss0(rl)), lossA(b)(lossA(lh)(lloss(l).head))))
              (bi.lm.append(lloss(l).head, ab.backward(lossA(lh)(ploss1(ploss0(rl)))))))(
              lLoss(nb.backward(ploss1(ploss0(rl))) :: lloss(ploss1(rl))))
          })
      }
    })

  override def scanLeft[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]) = curry_(curry_(scanLeftUC[A, B]))

  def scanRightUC[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]):
  BEval[((((A => B => B), B), scala.List[A])) => scala.List[B]] =
    aEval[(((A => B => B), B), scala.List[A]), scala.List[B]](p => {
      val abb = peval(peval(p)._1)._1
      val b = peval(peval(p)._1)._2
      leval(peval(p)._2) match {
        case Nil =>
          (listEval(List(b)), l => lossP(lossP(aInfo(ai, aInfo(bi, bi)).lm.zero)(lloss(l).head))(listInfo(ai).lm.zero))
        case lh :: lt =>
          val lb = aeval(scanRightUC[A, B]).forward(productEval(productEval(abb, b), listEval(lt)))
          val bb = aeval(abb).forward(lh)
          val nb = aeval(bb.eb).forward(leval(lb.eb).head)
          (listEval(nb.eb :: leval(lb.eb)), l => {
            val rl = lb.backward(lLoss(lloss(l).tail))
            lossP(lossP(
              aInfo(ai, aInfo(bi, bi)).lm.append(ploss0(ploss0(rl)),
                lossA(lh)(lossA(leval(lb.eb).head)(lloss(l).head))))(
              bi.lm.append(ploss1(ploss0(rl)), nb.backward(lloss(l).head))))(
              lLoss(bb.backward(lossA(leval(lb.eb).head)(lloss(l).head)) :: lloss(ploss1(rl))))
          })
      }
    })

  override def scanRight[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]) = curry_(curry_(scanRightUC[A, B]))
}

object BEvalList {
  implicit def apply = new BEvalList {}
}