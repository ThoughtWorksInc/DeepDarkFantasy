package com.thoughtworks.DDF.Lang

case class NextLang[Info[_], Repr[_], Arg](base: Lang[Info, Repr])(implicit argi: Info[Arg]) extends
  Lang[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] {

  override def ArrDomInfo[A, B] = x => iconv(base.ArrDomInfo(base.ArrRngInfo(x)))

  override def ArrRngInfo[A, B] = x => iconv(base.ArrRngInfo(base.ArrRngInfo(x)))

  def rconv[X]: Repr[X] => Either[Repr[X], Repr[Arg => X]] = x => Left(x)

  def iconv[X]: Info[X] => Info[Arg => X] = x => base.ArrowInfo[Arg, X](argi, x)

  override implicit def ProdInfo[A, B](implicit ai: Info[(Arg) => A], bi: Info[(Arg) => B]) =
    iconv(base.ProdInfo(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def ProdFstInfo[A, B] = p => iconv(base.ProdFstInfo(base.ArrRngInfo(p)))

  override def ProdSndInfo[A, B] = p => iconv(base.ProdSndInfo(base.ArrRngInfo(p)))

  override def DoubleInfo = iconv(base.DoubleInfo)

  override implicit def SumInfo[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    iconv(base.SumInfo(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def SumLeftInfo[A, B] = ab => iconv(base.SumLeftInfo(base.ArrRngInfo(ab)))

  override def SumRightInfo[A, B] = ab => iconv(base.SumRightInfo(base.ArrRngInfo(ab)))

  override def ArrowInfo[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    iconv(base.ArrowInfo[A, B](base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override implicit def ListInfo[A](implicit ai: Info[Arg => A]) = iconv(base.ListInfo(base.ArrRngInfo(ai)))

  override def ListElmInfo[A](implicit lai: Info[Arg => List[A]]) = iconv(base.ListElmInfo(base.ArrRngInfo(lai)))

  override implicit def UnitInfo = iconv(base.UnitInfo)

  override def mkUnit = rconv(base.mkUnit)

  override def left[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.left(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def right[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.right(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def sumMatch[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]) =
    rconv(base.sumMatch(base.ArrRngInfo(ai), base.ArrRngInfo(bi), base.ArrRngInfo(ci)))

  override def I[A](implicit ai: Info[Arg => A]) = rconv(base.I(base.ArrRngInfo(ai)))

  override def S[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]) =
    rconv(base.S(base.ArrRngInfo(ai), base.ArrRngInfo(bi), base.ArrRngInfo(ci)))

  override def Y[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.Y(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def K[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.K(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def mkProd[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.mkProd(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def fst[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.fst(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def snd[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.snd(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def curry[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]) =
    rconv(base.curry(base.ArrRngInfo(ai), base.ArrRngInfo(bi), base.ArrRngInfo(ci)))

  override def uncurry[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]) =
    rconv(base.uncurry(base.ArrRngInfo(ai), base.ArrRngInfo(bi), base.ArrRngInfo(ci)))

  override def C[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]) =
    rconv(base.C(base.ArrRngInfo(ai), base.ArrRngInfo(bi), base.ArrRngInfo(ci)))

  override def Nil[A](implicit ai: Info[Arg => A]) = rconv(base.Nil(base.ArrRngInfo(ai)))

  override def Cons[A](implicit ai: Info[Arg => A]) =
    rconv(base.Cons(base.ArrRngInfo(ai)))

  override def listMatch[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.listMatch(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def listMap[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.listMap(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def LitD = d => rconv(base.LitD(d))

  override def PlusD = rconv(base.PlusD)

  override def MultD = rconv(base.MultD)

  override def W[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.W(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def B[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]) =
    rconv(base.B(base.ArrRngInfo(ai), base.ArrRngInfo(bi), base.ArrRngInfo(ci)))

  def lift[X]: Repr[X] => Repr[Arg => X] = r => base.app(base.K[X, Arg](base.ReprInfo(r), argi))(r)

  def collapse[X]: Either[Repr[X], Repr[Arg => X]] => Repr[Arg => X] = {
    case Right(x) => x
    case Left(x) => lift(x)
  }

  override def app[A, B]:
  Either[Repr[A => B], Repr[Arg => A => B]] =>
    Either[Repr[A], Repr[Arg => A]] =>
      Either[Repr[B], Repr[Arg => B]] = f => x => (f, x) match {
    case (Left(l), Left(r)) => Left(base.app(l)(r))
    case (Right(l), Right(r)) =>
      Right(base.app(base.app(base.S[Arg, A, B](
        argi,
        base.ArrDomInfo(base.ArrRngInfo(base.ReprInfo(l))),
        base.ArrRngInfo(base.ArrRngInfo(base.ReprInfo(l)))))(l))(r))
    case (Left(l), Right(r)) => app(Right(lift(l)))(Right(r))
    case (Right(l), Left(r)) => app(Right(l))(Right(lift(r)))
  }

  override def ReprInfo[A]: Either[Repr[A], Repr[Arg => A]] => Info[Arg => A] = {
    case Left(x) => base.ArrowInfo(argi, base.ReprInfo(x))
    case Right(x) => base.ReprInfo(x)
  }

  def in = Right(base.I[Arg])

  override def litB = b => rconv(base.litB(b))

  override implicit def ite[A](implicit ai: Info[Arg => A]) = rconv(base.ite(base.ArrRngInfo(ai)))

  override def BoolInfo: Info[Arg => Boolean] = iconv(base.BoolInfo)
}
