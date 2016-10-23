package com.thoughtworks.DDF.Combinators

trait CombExt[Info[_], Repr[_]] extends Comb[Info, Repr] {
  val comb: Comb[Info, Repr]

  override def I[A](implicit ai: Info[A]) = comb.I

  override def B[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = comb.B

  override def S[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = comb.S

  override def W[A, B](implicit ai: Info[A], bi: Info[B]) = comb.W

  override def App[A, B](implicit ai: Info[A], bi: Info[B]) = comb.App

  override def C[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = comb.C

  override def K[A, B](implicit ai: Info[A], bi: Info[B]) = comb.K

  override def Let[A, B](implicit ai: Info[A], bi: Info[B]) = comb.Let

  override def Y[A, B](implicit ai: Info[A], bi: Info[B]) = comb.Y

  override def app[A, B] = comb.app

  override implicit def arrowInfo[A, B](implicit ai: Info[A], bi: Info[B]) = comb.arrowInfo

  override def arrowDomainInfo[A, B] = comb.arrowDomainInfo

  override def arrowRangeInfo[A, B] = comb.arrowRangeInfo

  override def reprInfo[A] = comb.reprInfo

}

object CombExt {
  implicit def apply[Info[_], Repr[_]](implicit combinator: Comb[Info, Repr]) = new CombExt[Info, Repr] {
    override val comb: Comb[Info, Repr] = combinator
  }
}