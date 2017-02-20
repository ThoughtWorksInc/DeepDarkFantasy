package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.Arr

trait OptionMatch extends OptionType with Arr {
  def optionMatch[A <: Type: Kind, B <: Type: Kind]: Option[A] ~>: B ~>: (A ~>: B) ~>: B

  final def optionMatch_[A <: Type: Kind, B <: Type: Kind](oa: Option[A]): B ~>: (A ~>: B) ~>: B =
    app(optionMatch[A, B])(oa)

  final def optionMatch__[A <: Type: Kind, B <: Type: Kind](oa: Option[A])(b: B) = app(optionMatch_[A, B](oa))(b)

  final def optionMatch___[A <: Type: Kind, B <: Type: Kind](oa: Option[A])(b: B)(f: A ~>: B) =
    app(optionMatch__(oa)(b))(f)
}
