package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm, NextLang}

trait GPair[A, B] extends Gradient[(A, B)] {
  def combine[C]: Stream[C] => Stream[C] => Stream[C] = {
    case Stream.Empty => r => r
    case lh #:: lt => {
      case Stream.Empty => lh #:: lt
      case rh #:: rt => lh #:: rh #:: combine(lt)(rt)
    }
  }

  implicit val AG: Gradient[A]

  implicit val BG: Gradient[B]

  override implicit def GInfo: LangInfoG[(A, B)] = ltl.prodInfo(AG.GInfo, BG.GInfo)

  override def constG: LangTerm[(A, B)] = ltl.mkProd__(AG.constG)(BG.constG)

  implicit val ai = AG.GInfo

  implicit val bi = BG.GInfo

  override val GCDS: Stream[GCD] =
    combine(AG.GCDS.map(x => new GCD {
      override val gc: LangTerm[Double => (A, B)] = ltl.B__(ltl.C__(ltl.mkProd[A, B])(BG.constG))(x.gc)
      override val gd: LangTerm[((A, B)) => Double] = ltl.B__(x.gd)(ltl.zro[A, B])
    }))(BG.GCDS.map(x => new GCD {
      override val gc: LangTerm[Double => (A, B)] = ltl.B__(ltl.mkProd_[A, B](AG.constG))(x.gc)
      override val gd: LangTerm[((A, B)) => Double] = ltl.B__(x.gd)(ltl.fst[A, B])
    }))
}

object GPair {
  implicit def apply[A, B](implicit ag: Gradient[A], bg: Gradient[B]) = new GPair[A, B] {
    override implicit val AG: Gradient[A] = ag

    override implicit val BG: Gradient[B] = bg
  }
}