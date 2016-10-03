package com.thoughtworks.DDF

import scalaz.Monoid

trait MonoidUnit extends Monoid[Unit] {
  override def zero: Unit = ()

  override def append(f1: Unit, f2: => Unit): Unit = ()
}

object MonoidUnit {
  implicit def apply = new MonoidUnit {}
}
