package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.Arr

trait StreamMatch extends StreamType with Arr {
  def streamMatch[A <: Type: Kind, B <: Type: Kind]: Stream[A] ~>: B ~>: (A ~>: Stream[A] ~>: B) ~>: B

  def streamMatch_[A <: Type: Kind, B <: Type: Kind](sa: Stream[A]) = app(streamMatch[A, B])(sa)

  final def streamMatch__[A <: Type: Kind, B <: Type: Kind](sa: Stream[A])(b: B) = app(streamMatch_[A, B](sa))(b)

  final def streamMatch___[A <: Type: Kind, B <: Type: Kind](sa: Stream[A])(b: B)(f: A ~>: Stream[A] ~>: B) =
    app(streamMatch__[A, B](sa)(b))(f)
}
