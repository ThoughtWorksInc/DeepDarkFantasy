package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.Arr

trait OptionMatch extends OptionType with Arr {
  def optionMatch[A <: Type: Kind, B <: Type: Kind]: Option[A] ~>: B ~>: (A ~>: B) ~>: B

  final def optionMatch_[A <: Type: Kind, B <: Type: Kind](oa: Option[A]): B ~>: (A ~>: B) ~>: B =
    app(optionMatch[A, B])(oa)

  final def optionMatch__[A, B]: Repr[scala.Option[A]] => Repr[B] => Repr[(A => B) => B] = oa => b =>
    app(optionMatch_(oa)(reprInfo(b)))(b)

  final def optionMatch___[A, B]: Repr[scala.Option[A]] => Repr[B] => Repr[A => B] => Repr[B] = oa => b =>
    app(optionMatch__(oa)(b))
}
