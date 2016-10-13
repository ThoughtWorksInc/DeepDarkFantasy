package com.thoughtworks.DDF

trait CommutativeMonoidUnit extends CommutativeMonoid[Unit] {
  override def zero: Unit = ()

  override def append(f1: Unit, f2: => Unit): Unit = ()
}

object CommutativeMonoidUnit {
  implicit def apply = new CommutativeMonoidUnit {}
}
