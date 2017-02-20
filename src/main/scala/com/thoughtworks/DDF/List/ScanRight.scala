package com.thoughtworks.DDF.List

trait ScanRight extends ListMin {
  def scanRight[A <: Type: Kind, B <: Type: Kind]: (A ~>: B ~>: B) ~>: B ~>: List[A] ~>: List[B]

  final def scanRight_[A <: Type: Kind, B <: Type: Kind](f: A ~>: B ~>: B) = app(scanRight[A, B])(f)

  final def scanRight__[A <: Type: Kind, B <: Type: Kind](f: A ~>: B ~>: B)(b: B) = app(scanRight_(f))(b)

  final def scanRight___[A <: Type: Kind, B <: Type: Kind](f: A ~>: B ~>: B)(b: B)(l: List[A]) =
    app(scanRight__(f)(b))(l)
}
