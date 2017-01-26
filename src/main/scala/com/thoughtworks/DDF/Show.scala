package com.thoughtworks.DDF

sealed trait Show

case class ShowAtom(s: String) extends Show {
  override def toString: String = s
}
case class ShowApp(f: String, x: Seq[Show]) extends Show {
  override def toString: String = "(" + f.toString + x.foldRight(")")((l, r) => " " + l + r)
}

object Show {
  def apply(s: String): Show = ShowAtom(s)

  def apply(f: Show)(x: Show): Show = f match {
    case ShowAtom(str) => ShowApp(str, Seq(x))
    case ShowApp(str, r) => ShowApp(str, r ++ Seq(x))
  }
}