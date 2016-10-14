package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.{ArrowLoss, BEvalArrow}
import com.thoughtworks.DDF.Combinators.BEvalComb
import com.thoughtworks.DDF.Product.BEvalProduct
import com.thoughtworks.DDF.{CommutativeMonoid, BEval, BEvalCase, Loss, LossCase}

import scalaz.Leibniz._

trait BEvalList extends ListRepr[Loss, BEval] with BEvalArrow with BEvalProduct {

  case class ListLC[A]() extends LossCase[List[A]] {
    override type ret = Loss[A]
  }

  case class ListDEC[A]() extends BEvalCase[List[A]] {
    override type ret = List[BEval[A]]
  }

  def leval[A](e: BEval[List[A]]): List[BEval[A]] = witness(e.ec.unique(ListDEC[A]()))(e.eca)

  def listEval[A](l: List[BEval[A]])(implicit ai: Loss[A]): BEval[List[A]] = new BEval[List[A]] {
    override def eca: ec.ret = l

    override def eval: List[A] = l.map(_.eval)

    override val ec: BEvalCase.Aux[List[A], List[BEval[A]]] = ListDEC()

    override val loss: Loss[List[A]] = listInfo(ai)
  }

  override implicit def listInfo[A](implicit ai: Loss[A]): Loss.Aux[List[A], List[ai.loss]] = new Loss[List[A]] {

    override def convert: List[A] => BEval[List[A]] = la => listEval[A](la.map(ai.convert))

    override val lc: LossCase.Aux[List[A], Loss[A]] = ListLC()

    override def lca: lc.ret = ai

    override type ret = List[ai.loss]

    override def m: CommutativeMonoid[loss] = new CommutativeMonoid[loss] {
      override def zero: loss = scala.List()

      override def append(f1: loss, f2: => loss): loss =
        if (f1.length > f2.length) append(f2, f1)
        else f1.zip(f2).map(p => ai.m.append(p._1, p._2)) ++ f2.drop(f1.length)
    }

    override def update(x: List[A])(rate: Double)(l: loss): List[A] = x.zip(l).map(p => ai.update(p._1)(rate)(p._2))
  }

  override def listElmInfo[A](implicit lai: Loss[List[A]]): Loss[A] = witness(lai.lc.unique(ListLC[A]()))(lai.lca)

  override def nil[A](implicit ai: Loss[A]): BEval[List[A]] = listEval(scala.List())

  override def cons[A](implicit ai: Loss[A]): BEval[A => List[A] => List[A]] =
    arrowEval[A, List[A] => List[A], ai.loss, ArrowLoss[List[A], List[ai.loss]]](a =>
      (arrowEval[List[A], List[A], List[ai.loss], List[ai.loss]](la =>
        (listEval(a :: leval(la)), l => l.tail)), _.mapReduce(_ => l => l.head)(ai.m)))(
      ai, arrowInfo(listInfo(ai), listInfo(ai)))

  private def comb = BEvalComb.apply

  override def listMatch[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[List[A] => B => (A => List[A] => B) => B] =
    arrowEval[
      List[A],
      B => (A => List[A] => B) => B,
      List[ai.loss],
      ArrowLoss[B, ArrowLoss[A => List[A] => B, bi.loss]]](l => leval(l) match {
      case Nil => (comb.K[B, A => List[A] => B], l => List())
      case lh :: lt => (app(comb.K[(A => List[A] => B) => B, B])(app(app(comb.C[A => List[A] => B, List[A], B])(app(app(
        comb.C[A => List[A] => B, A, List[A] => B])(
        comb.I[A => List[A] => B]))(lh)))(listEval(lt))),
        _.mapReduce(b => _.mapReduce(alab => l => {
          val lab = aBEval(alab).forward(lh)
          val b = aBEval(lab.eb).forward(listEval(lt))
          lab.backward(ArrowLoss(listEval(lt))(l)) +: b.backward(l)
        })(listInfo(ai).m))(listInfo(ai).m))
    })

  override def listMap[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[(A => B) => List[A] => List[B]] =
    arrowEval[A => B, List[A] => List[B], ArrowLoss[A, bi.loss], ArrowLoss[List[A], List[bi.loss]]](ab =>
      (arrowEval[List[A], List[B], List[ai.loss], List[bi.loss]](la => {
        val lb = leval(la).map(x => aBEval(ab).forward(x))
        (listEval(lb.map(_.eb)), l => lb.zip(l).map(x => x._1.backward(x._2)))
      }), _.mapReduce(la => l => leval(la).zip(l).map(p => ArrowLoss(p._1)(p._2)).
        foldRight(arrowInfo(ai, bi).m.zero)((x, y) => arrowInfo(ai, bi).m.append(x, y)))(arrowInfo(ai, bi).m)))

  override def reverse[A](implicit ai: Loss[A]): BEval[List[A] => List[A]] =
    arrowEval[List[A], List[A], List[ai.loss], List[ai.loss]](la => (listEval(leval(la).reverse), _.reverse))

  override def foldRight[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[(A => B => B) => B => List[A] => B] =
    arrowEval[
      (A => B => B),
      B => List[A] => B,
      ArrowLoss[A, ArrowLoss[B, bi.loss]],
      ArrowLoss[B, ArrowLoss[List[A], bi.loss]]](abb =>
      (arrowEval[B, List[A] => B, bi.loss, ArrowLoss[List[A], bi.loss]](b =>
        (arrowEval[List[A], B, List[ai.loss], bi.loss](la => leval(la) match {
          case Nil => (b, _ => scala.List())
          case lh :: lt =>
            val blab = aBEval(foldRight[A, B]).forward(abb)
            val lab = aBEval(blab.eb).forward(b)
            val nb = aBEval(lab.eb).forward(listEval(lt))
            val bb = aBEval(abb).forward(lh)
            val finb = aBEval(bb.eb).forward(nb.eb)
            (finb.eb, bl => {
              val rb = finb.backward(bl)
              val aih = bb.backward(ArrowLoss(nb.eb)(bl))
              aih :: nb.backward(rb)
            })
        })(listInfo(ai), bi), _.mapReduce(la => l => leval(la) match {
          case Nil => bi.m.zero
          case lh :: lt =>
            val blab = aBEval(foldRight[A, B]).forward(abb)
            val lab = aBEval(blab.eb).forward(b)
            val nb = aBEval(lab.eb).forward(listEval(lt))
            val bb = aBEval(abb).forward(lh)
            val finb = aBEval(bb.eb).forward(nb.eb)
            lab.backward(ArrowLoss(listEval(lt))(finb.backward(l)))
        })(bi.m)))(bi, arrowInfo(listInfo(ai), bi)),
        _.mapReduce(b => _.mapReduce(la => l => leval(la) match {
          case Nil => arrowInfo(ai, arrowInfo(bi, bi)).m.zero
          case lh :: lt =>
            val blab = aBEval(foldRight[A, B]).forward(abb)
            val lab = aBEval(blab.eb).forward(b)
            val nb = aBEval(lab.eb).forward(listEval(lt))
            val bb = aBEval(abb).forward(lh)
            val finb = aBEval(bb.eb).forward(nb.eb)
            arrowInfo(ai, arrowInfo(bi, bi)).m.append(
              ArrowLoss[A, ArrowLoss[B, bi.loss]](lh)(ArrowLoss[B, bi.loss](nb.eb)(l)),
              blab.backward(ArrowLoss(b)(ArrowLoss(listEval(lt))(finb.backward(l)))))
        })(arrowInfo(ai, arrowInfo(bi, bi)).m))(arrowInfo(ai, arrowInfo(bi, bi)).m)))

  override def foldLeft[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[(A => B => A) => A => List[B] => A] =
    arrowEval[
      A => B => A,
      A => List[B] => A,
      ArrowLoss[A, ArrowLoss[B, ai.loss]],
      ArrowLoss[A, ArrowLoss[List[B], ai.loss]]](aba =>
      (arrowEval[A, List[B] => A, ai.loss, ArrowLoss[List[B], ai.loss]](a =>
        (arrowEval[List[B], A, List[bi.loss], ai.loss](lb => leval(lb) match {
          case Nil => (a, _ => List())
          case lh :: lt =>
            val ba = aBEval(aba).forward(a)
            val na = aBEval(ba.eb).forward(lh)
            val alba = aBEval(foldLeft[A, B]).forward(aba)
            val lba = aBEval(alba.eb).forward(na.eb)
            val res = aBEval(lba.eb).forward(listEval(lt))
            (res.eb, ai => na.backward(lba.backward(ArrowLoss(listEval(lt))(ai))) +: res.backward(ai))
        })(listInfo(bi), ai),
          _.mapReduce(lb => l =>
            leval(lb) match {
              case Nil => l
              case lh :: lt =>
                val ba = aBEval(aba).forward(a)
                val na = aBEval(ba.eb).forward(lh)
                val alba = aBEval(foldLeft[A, B]).forward(aba)
                val lba = aBEval(alba.eb).forward(na.eb)
                ba.backward(ArrowLoss(lh)(lba.backward(ArrowLoss(listEval(lt))(l))))
            })(ai.m)))(ai, arrowInfo(listInfo(bi), ai)),
        _.mapReduce(a => _.mapReduce(lb => l => leval(lb) match {
          case Nil => arrowInfo(ai, arrowInfo(bi, ai)).m.zero
          case lh :: lt =>
            val ba = aBEval(aba).forward(a)
            val na = aBEval(ba.eb).forward(lh)
            val alba = aBEval(foldLeft[A, B]).forward(aba)
            val lba = aBEval(alba.eb).forward(na.eb)
            arrowInfo(ai, arrowInfo(bi, ai)).m.append(
              ArrowLoss(a)(ArrowLoss(lh)(lba.backward(ArrowLoss(listEval(lt))(l)))),
              alba.backward(ArrowLoss(na.eb)(ArrowLoss(listEval(lt))(l))))
        })(arrowInfo(ai, arrowInfo(bi, ai)).m))(arrowInfo(ai, arrowInfo(bi, ai)).m)))

  override def listZip[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[List[A] => List[B] => List[(A, B)]] =
    arrowEval[
      List[A],
      List[B] => List[(A, B)],
      List[ai.loss],
      ArrowLoss[List[B], List[(ai.loss, bi.loss)]]](al =>
      (arrowEval[List[B], List[(A, B)], List[bi.loss], List[(ai.loss, bi.loss)]](bl =>
        (listEval(leval(al).zip(leval(bl)).map(p => productEval(p._1, p._2))), _.map(_._2))),
        _.mapReduce(_ => _.map(_._1))(listInfo(ai).m)))
}

object BEvalList {
  implicit def apply = new BEvalList {}
}