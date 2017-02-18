package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.Arr

trait ListMatch extends ListType with Arr {
  def listMatch[A <: Type: Kind, B <: Type: Kind]: List[A] ~>: B ~>: (A ~>: List[A] ~>: B) ~>: B

  final def listMatch_[A <: Type: Kind, B <: Type: Kind](l: List[A]): B ~>: (A ~>: List[A] ~>: B) ~>: B =
    app(listMatch[A, B])(l)

  final def listMatch__[A <: Type: Kind, B <: Type: Kind](l: List[A])(b: B): (A ~>: List[A] ~>: B) ~>: B =
    app(listMatch_[A, B](l))(b)

  final def listMatch___[A <: Type: Kind, B <: Type: Kind](l: List[A])(b: B)(f: A ~>: List[A] ~>: B): B =
    app(listMatch__(l)(b))(f)
}
