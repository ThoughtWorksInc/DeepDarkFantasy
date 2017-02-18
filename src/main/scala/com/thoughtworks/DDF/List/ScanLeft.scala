package com.thoughtworks.DDF.List

trait ScanLeft extends ListMin {
  def scanLeft[A <: Type: Kind, B <: Type: Kind]: (B ~>: A ~>: B) ~>: B ~>: List[A] ~>: List[B]

  final def scanLeft_[A <: Type: Kind, B <: Type: Kind](f: B ~>: A ~>: B) = app(scanLeft[A, B])(f)

  final def scanLeft__[A <: Type: Kind, B <: Type: Kind](f: B ~>: A ~>: B)(b: B) = app(scanLeft_(f))(b)

  final def scanLeft___[A <: Type: Kind, B <: Type: Kind](f: B ~>: A ~>: B)(b: B)(l: List[A]) = app(scanLeft__(f)(b))(l)
}
