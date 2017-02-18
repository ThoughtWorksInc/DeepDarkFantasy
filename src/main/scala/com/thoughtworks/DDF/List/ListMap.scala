package com.thoughtworks.DDF.List

trait ListMap extends ListMin {
  def listMap[A <: Type: Kind, B <: Type: Kind]: (A ~>: B) ~>: List[A] ~>: List[B]

  final def listMap_[A <: Type: Kind, B <: Type: Kind](f: A ~>: B): List[A] ~>: List[B] = app(listMap[A, B])(f)

  final def listMap__[A <: Type: Kind, B <: Type: Kind](f: A ~>: B)(l: List[A]): List[B] = app(listMap_(f))(l)
}
