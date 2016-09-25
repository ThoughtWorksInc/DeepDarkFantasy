package com.thoughtworks.DDF

import SimpleLanguage._

object ShowLanguage {
  case class Show[X](s: String)

  class ShowLanguage extends SimpleLanguage[Show] {
    override def app[A, B] = f => x => Show("(" + f.s + " " + x.s + ")")

    override def S[A, B, C](implicit at: NoInfo[A], bt: NoInfo[B], ct: NoInfo[C]) = Show("S")

    override def K[A, B](implicit at: NoInfo[A], bt: NoInfo[B]): Show[A => B => A] = Show("K")

    override def I[A](implicit at: NoInfo[A]): Show[A => A] = Show("I")

    override def LitD: Double => Show[Double] = d => Show(d.toString)

    override def PlusD: Show[Double => Double => Double] = Show("+")

    override def MultD: Show[Double => Double => Double] = Show("*")

    override def Y[A, B](implicit at: NoInfo[A], bt: NoInfo[B]): Show[((A => B) => (A => B)) => (A => B)] = Show("Y")

    override def mkPair[A, B](implicit at: NoInfo[A], bt: NoInfo[B]): Show[(A) => (B) => (A, B)] = Show("mkPair")

    override def snd[A, B](implicit at: NoInfo[A], bt: NoInfo[B]): Show[((A, B)) => B] = Show("snd")

    override def fst[A, B](implicit at: NoInfo[A], bt: NoInfo[B]): Show[((A, B)) => A] = Show("fst")

    override def left[A, B](implicit at: NoInfo[A], bt: NoInfo[B]): Show[(A) => Either[A, B]] = Show("left")

    override def right[A, B](implicit at: NoInfo[A], bt: NoInfo[B]): Show[(B) => Either[A, B]] = Show("right")

    override def sumMatch[A, B, C](implicit at: NoInfo[A], bt: NoInfo[B], ct: NoInfo[C]):
    Show[((A) => C) => ((B) => C) => (Either[A, B]) => C] = Show("sumMatch")
  }

}
