package com.thoughtworks

import scalaz.Monoid
import scala.language.existentials
import scala.language.higherKinds
import scalaz.Leibniz._

object DDF {

  trait Language[Type[_], Repr[_]] {
    def Arr[A, B]: Type[A] => Type[B] => Type[A => B]

    def ArrDom[A, B]: Type[A => B] => Type[A]

    def ArrRng[A, B]: Type[A => B] => Type[B]

    def Pair[A, B]: Type[A] => Type[B] => Type[(A, B)]

    def PairFst[A, B]: Type[(A, B)] => Type[A]

    def PairSnd[A, B]: Type[(A, B)] => Type[B]

    def ReprType[A]: Repr[A] => Type[A]

    def app[A, B]: Repr[A => B] => Repr[A] => Repr[B]

    def S[A, B, C](implicit at: Type[A], bt: Type[B], ct: Type[C]): Repr[(A => B => C) => (A => B) => A => C]

    def K[A, B](implicit at: Type[A], bt: Type[B]): Repr[A => B => A]

    def I[A](implicit at: Type[A]): Repr[A => A]

    def LitD: Double => Repr[Double]

    def PlusD: Repr[Double => Double => Double]

    def MultD: Repr[Double => Double => Double]

    def Y[A, B](implicit at: Type[A], bt: Type[B]): Repr[((A => B) => (A => B)) => (A => B)]

    def mkPair[A, B](implicit at: Type[A], bt: Type[B]): Repr[A => B => (A, B)]

    def fst[A, B](implicit at: Type[A], bt: Type[B]): Repr[((A, B)) => A]

    def snd[A, B](implicit at: Type[A], bt: Type[B]): Repr[((A, B)) => B]
  }

  final case class NoType[X]()

  implicit def NoTypeGen[X]: NoType[X] = NoType[X]()

  trait SimpleLanguage[Repr[_]] extends Language[NoType, Repr] {
    override def Arr[A, B]: NoType[A] => NoType[B] => NoType[A => B] = _ => _ => NoType()

    override def ArrDom[A, B]: NoType[A => B] => NoType[A] = _ => NoType()

    override def ArrRng[A, B]: NoType[A => B] => NoType[B] = _ => NoType()

    override def ReprType[A]: Repr[A] => NoType[A] = _ => NoType()

    override def Pair[A, B]: (NoType[A]) => (NoType[B]) => NoType[(A, B)] = _ => _ => NoType()

    override def PairFst[A, B]: (NoType[(A, B)]) => NoType[A] = _ => NoType()

    override def PairSnd[A, B]: (NoType[(A, B)]) => NoType[B] = _ => NoType()
  }

  case class Show[X](s: String)

  class ShowLanguage extends SimpleLanguage[Show] {
    override def app[A, B] = f => x => Show("(" + f.s + " " + x.s + ")")

    override def S[A, B, C](implicit at: NoType[A], bt: NoType[B], ct: NoType[C]) = Show("S")

    override def K[A, B](implicit at: NoType[A], bt: NoType[B]): Show[A => B => A] = Show("K")

    override def I[A](implicit at: NoType[A]): Show[A => A] = Show("I")

    override def LitD: Double => Show[Double] = d => Show(d.toString)

    override def PlusD: Show[Double => Double => Double] = Show("+")

    override def MultD: Show[Double => Double => Double] = Show("*")

    override def Y[A, B](implicit at: NoType[A], bt: NoType[B]): Show[((A => B) => (A => B)) => (A => B)] = Show("Y")

    override def mkPair[A, B](implicit at: NoType[A], bt: NoType[B]): Show[(A) => (B) => (A, B)] = Show("mkPair")

    override def snd[A, B](implicit at: NoType[A], bt: NoType[B]): Show[((A, B)) => B] = Show("snd")

    override def fst[A, B](implicit at: NoType[A], bt: NoType[B]): Show[((A, B)) => A] = Show("fst")
  }

  case class Next[Type[_], Repr[_], Arg](base: Language[Type, Repr])(implicit argt: Type[Arg]) extends
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

    def conv[X]: Repr[X] => Repr[Arg => X] = r => base.app(base.K[X, Arg](base.ReprType(r), argt))(r)

    def in: Repr[Arg => Arg] = base.I

    override def Y[A, B](implicit at: Type[Arg => A], bt: Type[Arg => B]):
    Repr[(Arg) => ((A => B) => A => B) => A => B] = conv(base.Y[A, B](base.ArrRng(at), base.ArrRng(bt)))

    override def mkPair[A, B](implicit at: Type[Arg => A], bt: Type[Arg => B]): Repr[Arg => A => B => (A, B)] =
      conv(base.mkPair[A, B](base.ArrRng(at), base.ArrRng(bt)))

    override def fst[A, B](implicit at: Type[(Arg) => A], bt: Type[(Arg) => B]): Repr[(Arg) => ((A, B)) => A] =
      conv(base.fst(base.ArrRng(at), base.ArrRng(bt)))

    override def snd[A, B](implicit at: Type[(Arg) => A], bt: Type[(Arg) => B]): Repr[(Arg) => ((A, B)) => B] =
      conv(base.snd(base.ArrRng(at), base.ArrRng(bt)))

    override def Pair[A, B]: (Type[(Arg) => A]) => (Type[(Arg) => B]) => Type[(Arg) => (A, B)] = a => b =>
      base.Arr(base.ArrDom(a))(base.Pair(base.ArrRng(a))(base.ArrRng(b)))

    override def PairFst[A, B]: Type[Arg => (A, B)] => Type[Arg => A] = p => base.Arr(base.ArrDom(p))(base.PairFst(base.ArrRng(p)))

    override def PairSnd[A, B]: Type[Arg => (A, B)] => Type[Arg => B] = p => base.Arr(base.ArrDom(p))(base.PairSnd(base.ArrRng(p)))
  }

  trait Loss[Self[_], X] {
    type loss

    def m: Monoid[loss]

    def unique[Y](l: Loss[Self, Y])(implicit ev: X === Y): loss === l.loss = /*enforced by user*/ force[Nothing, Any, loss, l.loss]
  }

  trait Arr[Self[_], X] {
    def ArrDom[A, B](implicit ev: X === (A => B)): Self[A]

    def ArrRng[A, B](implicit ev: X === (A => B)): Self[B]
  }

  trait Pair[Self[_], X] {
    def PairFst[A, B](implicit ev: X === (A, B)): Self[A]

    def PairSnd[A, B](implicit ev: X === (A, B)): Self[B]
  }

  trait APLoss[X] extends Loss[APLoss, X] with Arr[APLoss, X] with Pair[APLoss, X]

  object APLoss {
    type Aux[X, L] = APLoss[X] {type loss = L}
  }

  case class DLoss(x: Double)

  implicit def dLoss = new APLoss[Double] {
    override type loss = DLoss

    override def m: Monoid[loss] = new Monoid[DLoss] {
      override def zero: DLoss = DLoss(0.0)

      override def append(f1: DLoss, f2: => DLoss): DLoss = DLoss(f1.x + f2.x)
    }

    override def ArrDom[A, B](implicit ev: ===[Double, A => B]): APLoss[A] = throw new Exception("not ArrLoss")

    override def ArrRng[A, B](implicit ev: ===[Double, A => B]): APLoss[B] = throw new Exception("not ArrLoss")

    override def PairFst[A, B](implicit ev: ===[Double, (A, B)]): APLoss[A] = throw new Exception("not PairLoss")

    override def PairSnd[A, B](implicit ev: ===[Double, (A, B)]): APLoss[B] = throw new Exception("not PairLoss")
  }

  case class ArrLoss[A, BL](seq: Seq[(Eval[A], BL)])

  def ArrEq[A, B, C, D]: A === C => B === D => (A => B) === (C => D) = dom => rng =>
    dom.subst[Lambda[X => (A => B) === (X => D)]](rng.subst[Lambda[X => (A => B) === (A => X)]](refl[A => B]))

  def ArrDomEq[A, B, C, D]: (A => B) === (C => D) => A === C = _ => force[Nothing, Any, A, C]

  def ArrRngEq[A, B, C, D]: (A => B) === (C => D) => B === D = _ => force[Nothing, Any, B, D]

  implicit def arrLoss[A, B](implicit AL: APLoss[A], BL: APLoss[B]): APLoss.Aux[A => B, ArrLoss[A, BL.loss]] =
    new APLoss[A => B] {
      override type loss = ArrLoss[A, BL.loss]

      override def m: Monoid[ArrLoss[A, BL.loss]] = new Monoid[ArrLoss[A, BL.loss]] {
        override def zero: ArrLoss[A, BL.loss] = ArrLoss(Seq())

        override def append(f1: ArrLoss[A, BL.loss], f2: => ArrLoss[A, BL.loss]): ArrLoss[A, BL.loss] =
          ArrLoss(f1.seq ++ f2.seq)
      }

      override def ArrDom[C, D](implicit ev: ===[A => B, C => D]): APLoss[C] = ArrDomEq(ev).subst[APLoss](AL)

      override def ArrRng[C, D](implicit ev: ===[A => B, C => D]): APLoss[D] = ArrRngEq(ev).subst[APLoss](BL)

      override def PairFst[C, D](implicit ev: ===[(A) => B, (C, D)]): APLoss[C] = throw new Exception("not PairLoss")

      override def PairSnd[C, D](implicit ev: ===[(A) => B, (C, D)]): APLoss[D] = throw new Exception("not PairLoss")
    }

  def PairEq[A, B, C, D]: A === C => B === D => (A, B) === (C, D) = ac => bd =>
    ac.subst[Lambda[C => (A, B) === (C, D)]](bd.subst[Lambda[D => (A, B) === (A, D)]](refl[(A, B)]))

  def PairFstEq[A, B, C, D]: (A, B) === (C, D) => A === C = _ => force[Nothing, Any, A, C]

  def PairSndEq[A, B, C, D]: (A, B) === (C, D) => B === D = _ => force[Nothing, Any, B, D]

  implicit def pairLoss[A, B](implicit al: APLoss[A], bl: APLoss[B]) = new APLoss[(A, B)] {
    override type loss = (al.loss, bl.loss)

    override def m: Monoid[(al.loss, bl.loss)] = new Monoid[(al.loss, bl.loss)] {
      override def zero: (al.loss, bl.loss) = (al.m.zero, bl.m.zero)

      override def append(f1: (al.loss, bl.loss), f2: => (al.loss, bl.loss)): (al.loss, bl.loss) =
        (al.m.append(f1._1, f2._1), bl.m.append(f1._2, f2._2))
    }

    override def ArrDom[C, D](implicit ev: ===[(A, B), C => D]): APLoss[C] = throw new Exception("not ArrLoss")

    override def ArrRng[C, D](implicit ev: ===[(A, B), C => D]): APLoss[D] = throw new Exception("not ArrLoss")

    override def PairFst[C, D](implicit ev: ===[(A, B), (C, D)]): APLoss[C] = PairFstEq(ev).subst[APLoss](al)

    override def PairSnd[C, D](implicit ev: ===[(A, B), (C, D)]): APLoss[D] = PairSndEq(ev).subst[APLoss](bl)
  }

  trait Eval[X] {
    def aeval[A, B](a: Eval[A])(implicit ev: X === (A => B), AL: APLoss[A], BL: APLoss[B]): (Eval[B], BL.loss => AL.loss)

    def deval(implicit ev: X === Double): Double

    def peval[A, B](implicit ev: X === (A, B)): (Eval[A], Eval[B])

    def loss: APLoss[X]
  }

  def PairEval[A, B](a: Eval[A], b: Eval[B])(implicit l: APLoss[(A, B)]) = new Eval[(A, B)] {
    override def aeval[C, D](a: Eval[C])(
      implicit ev: ===[(A, B), C => D], CL: APLoss[C], DL: APLoss[D]): (Eval[D], DL.loss => CL.loss) = throw new Exception("not ArrEval")

    override def deval(implicit ev: ===[(A, B), Double]): Double = throw new Exception("not DEval")

    override def peval[C, D](implicit ev: ===[(A, B), (C, D)]): (Eval[C], Eval[D]) =
      (PairFstEq(ev).subst[Eval](a), PairSndEq(ev).subst[Eval](b))

    override def loss: APLoss[(A, B)] = l
  }

  def ArrEval[A, B, AL, BL](forward: Eval[A] => (Eval[B], BL => AL))(implicit al: APLoss.Aux[A, AL], bl: APLoss.Aux[B, BL]) =
    new Eval[A => B] {
      override def aeval[C, D](c: Eval[C])(
        implicit ev: (A => B) === (C => D), CL: APLoss[C], DL: APLoss[D]): (Eval[D], DL.loss => CL.loss) = {
        val ac = ArrDomEq(ev)
        val bd = ArrRngEq(ev)
        val f = forward(symm[Nothing, Any, A, C](ac).subst[Eval](c))
        (bd.subst[Eval](f._1), dl =>
          witness(symm[Nothing, Any, CL.loss, AL](CL.unique(al)(symm[Nothing, Any, A, C](ac))))(
            f._2(witness(DL.unique(bl)(symm[Nothing, Any, B, D](bd)))(dl))))
      }

      override def deval(implicit ev: ===[A => B, Double]): Double = throw new Exception("not DEval")

      override def loss: APLoss[A => B] = arrLoss(al, bl)

      override def peval[C, D](implicit ev: ===[(A) => B, (C, D)]): (Eval[C], Eval[D]) = throw new Exception("not PEval")
    }

  case class DEval(d: Double) extends Eval[Double] {
    val eval = d

    override def aeval[A, B](a: Eval[A])(implicit ev: ===[Double, A => B], AL: APLoss[A], BL: APLoss[B]):
    (Eval[B], BL.loss => AL.loss) = throw new Exception("not ArrEval")

    override def peval[A, B](implicit ev: ===[Double, (A, B)]): (Eval[A], Eval[B]) = throw new Exception("not PEval")

    override def deval(implicit ev: ===[Double, Double]): Double = d

    override def loss: APLoss[Double] = dLoss
  }

  class EvalL extends Language[APLoss, Eval] {
    override def Arr[A, B]: APLoss[A] => APLoss[B] => APLoss[A => B] = x => y => arrLoss(x, y)

    override def ArrDom[A, B]: APLoss[A => B] => APLoss[A] = _.ArrDom[A, B](refl[A => B])

    override def ArrRng[A, B]: APLoss[A => B] => APLoss[B] = _.ArrRng[A, B](refl[A => B])

    override def app[A, B] = f => x => f.aeval(x)(refl[A => B], x.loss, ArrRng(f.loss))._1

    override def S[A, B, C](implicit at: APLoss[A], bt: APLoss[B], ct: APLoss[C]): Eval[(A => B => C) => (A => B) => A => C] =
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

    override def K[A, B](implicit at: APLoss[A], bt: APLoss[B]): Eval[A => B => A] =
      ArrEval[A, B => A, at.loss, ArrLoss[B, at.loss]](a => (
        ArrEval[B, A, bt.loss, at.loss](_ => (a, _ => bt.m.zero))(bt, at),
        l => l.seq.map(_._2).fold(at.m.zero)((l, r) => at.m.append(l, r))))(at, arrLoss(bt, at))

    override def I[A](implicit at: APLoss[A]): Eval[A => A] = ArrEval[A, A, at.loss, at.loss](x => (x, y => y))(at, at)

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

    override def ReprType[A]: Eval[A] => APLoss[A] = _.loss

    override def Y[A, B](implicit at: APLoss[A], bt: APLoss[B]): Eval[((A => B) => A => B) => A => B] = {
      type abt = ArrLoss[A, bt.loss]
      ArrEval[(A => B) => A => B, A => B, ArrLoss[A => B, abt], abt](abab => {
        val ab = ArrEval[A, B, at.loss, bt.loss](a => app(abab)(app(Y[A, B])(abab)).aeval(a))(at, bt)
        (ab, abl => ArrLoss(abl.seq.map(p => (ab, ArrLoss(Seq(p))))))
      })
    }

    override def fst[A, B](implicit at: APLoss[A], bt: APLoss[B]): Eval[((A, B)) => A] =
      ArrEval[(A, B), A, (at.loss, bt.loss), at.loss](p => (p.peval(refl[(A, B)])._1, al => (al, bt.m.zero)))(pairLoss(at, bt), at)

    override def snd[A, B](implicit at: APLoss[A], bt: APLoss[B]): Eval[((A, B)) => B] =
      ArrEval[(A, B), B, (at.loss, bt.loss), bt.loss](p => (p.peval(refl[(A, B)])._2, bl => (at.m.zero, bl)))(pairLoss(at, bt), bt)

    override def mkPair[A, B](implicit at: APLoss[A], bt: APLoss[B]): Eval[(A) => (B) => (A, B)] =
      ArrEval[A, B => (A, B), at.loss, ArrLoss[B, (at.loss, bt.loss)]](a => (ArrEval[B, (A, B), bt.loss, (at.loss, bt.loss)](b =>
        (PairEval(a, b), _._2))(bt, pairLoss(at, bt)), _.seq.map(_._2._1).foldRight[at.loss](at.m.zero)((x, y) => at.m.append(x, y))))(
        at, arrLoss(bt, pairLoss(at, bt)))

    override def Pair[A, B]: (APLoss[A]) => (APLoss[B]) => APLoss[(A, B)] = a => b => pairLoss(a, b)

    override def PairFst[A, B]: (APLoss[(A, B)]) => APLoss[A] = _.PairFst(refl[(A, B)])

    override def PairSnd[A, B]: (APLoss[(A, B)]) => APLoss[B] = _.PairSnd(refl[(A, B)])
  }

  def Square[Type[_], Repr[_]](implicit td: Type[Double], lang: Language[Type, Repr]): Repr[Double => Double] = {
    val next = Next[Type, Repr, Double](lang)
    import next._
    app(app(MultD)(in))(in)
  }
}
