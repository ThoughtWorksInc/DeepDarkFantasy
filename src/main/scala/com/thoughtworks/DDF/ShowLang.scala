package com.thoughtworks.DDF

class ShowLang extends SimpleLang[Show] {
  override def app[A, B] = f => x => Show("(" + f.s + " " + x.s + ")")

  override def S[A, B, C](implicit at: NoInfo[A], bt: NoInfo[B], ct: NoInfo[C]) = Show("S")

  override def K[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("K")

  override def I[A](implicit at: NoInfo[A]) = Show("I")

  override def LitD = d => Show(d.toString)

  override def PlusD = Show("+")

  override def MultD = Show("*")

  override def Y[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("Y")

  override def mkProd[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("mkPair")

  override def snd[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("snd")

  override def fst[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("fst")

  override def left[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("left")

  override def right[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("right")

  override def sumMatch[A, B, C](implicit at: NoInfo[A], bt: NoInfo[B], ct: NoInfo[C]) = Show("sumMatch")

  override def B[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = Show("B")

  override def W[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("W")

  override def C[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = Show("C")

  override def curry[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = Show("curry")

  override def uncurry[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = Show("uncurry")

  override def Cons[A](implicit ai: NoInfo[A]) = Show("Cons")

  override def listMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("listMatch")

  override def Nil[A](implicit ai: NoInfo[A]) = Show("Nil")

  override def mkUnit = Show("mkUnit")

  override def listMap[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("listMap")
}
