package com.thoughtworks

import scalaz.Monoid
import scala.language.existentials
import scala.language.higherKinds
import scalaz.Leibniz._

object DDF {
object DeepLearning {
  trait Language[Type[_], Repr[_]] {
    def Arr[A, B] : Type[A] => Type[B] => Type[A => B]
    def ArrDom[A, B] : Type[A => B] => Type[A]
    def ArrRng[A, B] : Type[A => B] => Type[B]
    def ReprType[A] : Repr[A] => Type[A]
    def app[A, B] : Repr[A => B] => Repr[A] => Repr[B]
    def S[A, B, C](implicit at : Type[A], bt : Type[B], ct : Type[C]) : Repr[(A => B => C) => (A => B) => A => C]
    def K[A, B](implicit at : Type[A], bt : Type[B]) : Repr[A => B => A]
    def I[A](implicit at : Type[A]) : Repr[A => A]
    def LitD : Double => Repr[Double]
    def PlusD : Repr[Double => Double => Double]
    def MultD : Repr[Double => Double => Double]
  }
  final case class NoType[X]()
  implicit def NoTypeGen[X] : NoType[X] = NoType[X]()
  trait SimpleLanguage[Repr[_]] extends Language[NoType, Repr] {
    override def Arr[A, B]: NoType[A] => NoType[B] => NoType[A => B] = _ => _ => NoType()
    override def ArrDom[A, B]: NoType[A => B] => NoType[A] = _ => NoType()
    override def ArrRng[A, B]: NoType[A => B] => NoType[B] = _ => NoType()
    override def ReprType[A]: Repr[A] => NoType[A] = _ => NoType()
  }

  case class Show[X](s : String)
  class ShowLanguage extends SimpleLanguage[Show] {
    override def app[A, B] = f => x => Show("(" + f.s + " " + x.s + ")")
    override def S[A, B, C](implicit at: NoType[A], bt: NoType[B], ct: NoType[C]) = Show("S")
    override def K[A, B](implicit at: NoType[A], bt: NoType[B]): Show[A => B => A] = Show("K")
    override def I[A](implicit at: NoType[A]): Show[A => A] = Show("I")
    override def LitD: Double => Show[Double] = d => Show(d.toString)
    override def PlusD: Show[Double => Double => Double] = Show("+")
    override def MultD: Show[Double => Double => Double] = Show("*")
  }
  class Next[Type[_], Repr[_], Arg](base : Language[Type, Repr])(implicit argt : Type[Arg]) extends
    Language[Lambda[X => Type[Arg => X]], Lambda[X => Repr[Arg => X]]] {
    override def Arr[A, B]: Type[Arg => A] => Type[Arg => B] => Type[Arg => A => B] = l => r =>
      base.Arr(base.ArrDom(l))(base.Arr(base.ArrRng(l))(base.ArrRng(r)))

    override def ArrDom[A, B]: Type[Arg => A => B] => Type[Arg => A] = x =>
      base.Arr(base.ArrDom(x))(base.ArrDom(base.ArrRng(x)))

    override def ArrRng[A, B]: Type[Arg => A => B] => Type[Arg => B] = x =>
      base.Arr(base.ArrDom(x))(base.ArrRng(base.ArrRng(x)))

    override def S[A, B, C](implicit at: Type[Arg => A], bt: Type[Arg => B], ct: Type[Arg => C]):
    Repr[Arg => (A => B => C) => (A => B) => A => C] = conv(base.S[A, B, C](base.ArrRng(at), base.ArrRng(bt), base.ArrRng(ct)))

    override def K[A, B](implicit at: Type[Arg => A], bt: Type[Arg => B]): Repr[Arg => A => B => A] =
      conv(base.K[A, B](base.ArrRng(at), base.ArrRng(bt)))

    override def I[A](implicit at: Type[Arg => A]): Repr[Arg => A => A] = conv(base.I[A](base.ArrRng(at)))

    override def LitD: Double => Repr[Arg => Double] = x => conv(base.LitD(x))

    override def PlusD: Repr[Arg => Double => Double => Double] = conv(base.PlusD)

    override def MultD: Repr[Arg => Double => Double => Double] = conv(base.MultD)

    override def app[A, B] = f => x => base.app(base.app(
      base.S[Arg, A, B](argt, base.ArrRng(ReprType(x)), base.ArrRng(base.ArrRng(ReprType(f)))))(f))(x)

    override def ReprType[A]: Repr[Arg => A] => Type[Arg => A] = r => base.ReprType(r)

    def conv[X] : Repr[X] => Repr[Arg => X] = r => base.app(base.K[X, Arg](base.ReprType(r), argt))(r)
  }

  abstract class Loss[X] {
    type loss
    def m : Monoid[loss]
    def unique[Y](l : Loss[Y])(implicit ev : X === Y) : loss === l.loss = /*enforced by user*/ force[Nothing, Any, loss, l.loss]
    def ArrDom[A, B](implicit ev : X === (A => B)) : Loss[A]
    def ArrRng[A, B](implicit ev : X === (A => B)) : Loss[B]
  }

  object Loss {
    type Aux[X, L] = Loss[X] { type loss = L }
  }

  case class DLoss(x : Double)
  implicit def dLoss = new Loss[Double] {
    override type loss = DLoss
    override def m: Monoid[loss] = new Monoid[DLoss] {
      override def zero: DLoss = DLoss(0.0)
      override def append(f1: DLoss, f2: => DLoss): DLoss = DLoss(f1.x + f2.x)
    }
    override def ArrDom[A, B](implicit ev: ===[Double, A => B]): Loss[A] = throw new Exception("not ArrLoss")
    override def ArrRng[A, B](implicit ev: ===[Double, A => B]): Loss[B] = throw new Exception("not ArrLoss")
  }

  case class ArrLoss[A, BL](seq : Seq[(Eval[A], BL)])
  implicit def arrLoss[A, B](implicit AL : Loss[A], BL : Loss[B]) : Loss.Aux[A => B, ArrLoss[A, BL.loss]] =
    new Loss[A => B] {
      override type loss = ArrLoss[A, BL.loss]
      override def m: Monoid[ArrLoss[A, BL.loss]] = new Monoid[ArrLoss[A, BL.loss]] {
        override def zero: ArrLoss[A, BL.loss] = ArrLoss(Seq())
        override def append(f1: ArrLoss[A, BL.loss], f2: => ArrLoss[A, BL.loss]): ArrLoss[A, BL.loss] =
          ArrLoss(f1.seq ++ f2.seq)
      }
      override def ArrDom[C, D](implicit ev: ===[A => B, C => D]): Loss[C] = ArrDomEq(ev).subst[Loss](AL)
      override def ArrRng[C, D](implicit ev: ===[A => B, C => D]): Loss[D] = ArrRngEq(ev).subst[Loss](BL)
    }

  abstract class Eval[X] {
    def aeval[A, B](a : Eval[A])(implicit ev : X === (A => B), AL : Loss[A], BL : Loss[B]) : (Eval[B], BL.loss => AL.loss)
    def deval(implicit ev : X === Double) : Double
    def loss : Loss[X]
  }

  def ArrEq[A, B, C, D] : A === C => B === D => (A => B) === (C => D) = dom => rng =>
    dom.subst[Lambda[X => (A => B) === (X => D)]](rng.subst[Lambda[X => (A => B) === (A => X)]](refl[A => B]))
  def ArrDomEq[A, B, C, D] : (A => B) === (C => D) => A === C = _ => force[Nothing, Any, A, C]
  def ArrRngEq[A, B, C, D] : (A => B) === (C => D) => B === D = _ => force[Nothing, Any, B, D]

  def ArrEval[A, B, AL, BL](forward : Eval[A] => (Eval[B], BL => AL))(implicit al : Loss.Aux[A, AL], bl : Loss.Aux[B, BL]) =
    new Eval[A => B] {
      override def aeval[C, D](c : Eval[C])(
        implicit ev: (A => B) === (C => D), CL: Loss[C], DL: Loss[D]): (Eval[D], DL.loss => CL.loss) = {
        val ac = ArrDomEq(ev)
        val bd = ArrRngEq(ev)
        val f = forward(symm[Nothing, Any, A, C](ac).subst[Eval](c))
        (bd.subst[Eval](f._1), dl =>
          witness(symm[Nothing, Any, CL.loss, AL](CL.unique(al)(symm[Nothing, Any, A, C](ac))))(
            f._2(witness(DL.unique(bl)(symm[Nothing, Any, B, D](bd)))(dl))))
      }
      override def deval(implicit ev: ===[A => B, Double]): Double = throw new Exception("cannot call deval")
      override def loss: Loss[A => B] = arrLoss(al, bl)
    }

  case class DEval(d : Double) extends Eval[Double] {
    val eval = d
    override def aeval[A, B](a: Eval[A])(
      implicit ev: ===[Double, A => B], AL: Loss[A], BL: Loss[B]): (Eval[B], BL.loss => AL.loss) =
      throw new Exception("cannot call aeval")
    override def deval(implicit ev: ===[Double, Double]): Double = d
    override def loss: Loss[Double] = dLoss
  }

  class EvalL extends Language[Loss, Eval] {
    override def Arr[A, B]: Loss[A] => Loss[B] => Loss[A => B] = x => y => arrLoss(x, y)
    override def ArrDom[A, B]: Loss[A => B] => Loss[A] = _.ArrDom[A, B](refl[A => B])
    override def ArrRng[A, B]: Loss[A => B] => Loss[B] = _.ArrRng[A, B](refl[A => B])
    override def app[A, B] = f => x => f.aeval(x)(refl[A => B], x.loss, ArrRng(f.loss))._1
    override def S[A, B, C](implicit at: Loss[A], bt: Loss[B], ct: Loss[C]): Eval[(A => B => C) => (A => B) => A => C] =
      ArrEval[
        A => B => C,
        (A => B) => A => C,
        ArrLoss[A, ArrLoss[B, ct.loss]],
        ArrLoss[A => B, ArrLoss[A, ct.loss]]](
        abc => (ArrEval[A => B, A => C, ArrLoss[A, bt.loss], ArrLoss[A, ct.loss]](
          ab => (ArrEval[A, C, at.loss, ct.loss](a => {
            val bc = abc.aeval[A, B => C](a)(refl[A => B => C], at, arrLoss(bt, ct))
            val b = ab.aeval[A, B](a)(refl[A => B], at, bt)
            val c = bc._1.aeval[B, C](b._1)(refl[B => C], bt, ct)
            (c._1, l => at.m.append(bc._2(ArrLoss(Seq((b._1, l)))), b._2(c._2(l))))
          })(at, ct), l => ArrLoss(l.seq.map(x => (x._1, {
            val bc = abc.aeval[A, B => C](x._1)(refl[A => B => C], at, arrLoss(bt, ct))
            val b = ab.aeval[A, B](x._1)(refl[A => B], at, bt)
            val c = bc._1.aeval[B, C](b._1)(refl[B => C], bt, ct)
            c._2(x._2)
          }))))), l => ArrLoss(l.seq.flatMap(x => x._2.seq.map(y => (
          y._1, ArrLoss(Seq((x._1.aeval(y._1)(refl[A => B], at, bt)._1, y._2)))))))))
    override def K[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[A => B => A] =
      ArrEval[A, B => A, at.loss, ArrLoss[B, at.loss]](a => (
        ArrEval[B, A, bt.loss, at.loss](_ => (a, _ => bt.m.zero))(bt, at),
        l => l.seq.map(_._2).fold(at.m.zero)((l, r) => at.m.append(l, r))))(at, arrLoss(bt, at))
    override def I[A](implicit at: Loss[A]): Eval[A => A] = ArrEval[A, A, at.loss, at.loss](x => (x, y => y))(at, at)
    override def LitD: Double => Eval[Double] = DEval
    override def PlusD: Eval[Double => Double => Double] =
      ArrEval[Double, Double => Double, DLoss, ArrLoss[Double, DLoss]](l =>
        (ArrEval[Double, Double, DLoss, DLoss](
          r => (DEval(l.deval + r.deval), rl => rl)),
          ll => DLoss(ll.seq.map(_._2.x).sum)))
    override def MultD: Eval[Double => Double => Double] =
      ArrEval[Double, Double => Double, DLoss, ArrLoss[Double, DLoss]](l =>
        (ArrEval[Double, Double, DLoss, DLoss](
          r => (DEval(l.deval * r.deval), rl => DLoss(l.deval * rl.x))),
          ll => DLoss(ll.seq.map(l => l._1.deval * l._2.x).sum)))
    override def ReprType[A]: Eval[A] => Loss[A] = _.loss
  }
}

}
