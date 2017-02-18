package com.thoughtworks.DDF.List

trait FoldLeft extends ListMin {
  def foldLeft[A <: Type: Kind, B <: Type: Kind]: (A ~>: B ~>: A) ~>: A ~>: List[B] ~>: A

  final def foldLeft_[A <: Type: Kind, B <: Type: Kind](f: A ~>: B ~>: A): A ~>: List[B] ~>: A = app(foldLeft[A, B])(f)

  final def foldLeft__[A <: Type: Kind, B <: Type: Kind](f: A ~>: B ~>: A)(a: A): List[B] ~>: A = app(foldLeft_(f))(a)

  final def foldLeft___[A <: Type: Kind, B <: Type: Kind](f: A ~>: B ~>: A)(a: A)(l: List[B]) = app(foldLeft__(f)(a))(l)
}
