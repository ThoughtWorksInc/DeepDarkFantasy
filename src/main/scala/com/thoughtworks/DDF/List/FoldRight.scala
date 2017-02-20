package com.thoughtworks.DDF.List

trait FoldRight extends ListMin {
  def foldRight[A <: Type: Kind, B <: Type: Kind]: (A ~>: B ~>: B) ~>: B ~>: List[A] ~>: B

  final def foldRight_[A <: Type: Kind, B <: Type: Kind](f: A ~>: B ~>: B) = app(foldRight[A, B])(f)

  final def foldRight__[A <: Type: Kind, B <: Type: Kind](f: A ~>: B ~>: B)(b: B) = app(foldRight_[A, B](f))(b)

  final def foldRight___[A <: Type: Kind, B <: Type: Kind](f: A ~>: B ~>: B)(b: B)(l: List[A]) =
    app(foldRight__(f)(b))(l)
}
