package com.thoughtworks.DDF

sealed trait Show[X]

case class ShowLeaf[X](s: String) extends Show[X] {
  override def toString: String = s
}
case class ShowNode[X](pre: String, x: Seq[Show[X]], post: String, inter: String) extends Show[X] {
  override def toString: String = pre + x.map(_.toString).foldRight("")((l, r) => l + inter + r) + post
}