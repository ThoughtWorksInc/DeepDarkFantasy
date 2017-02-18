package com.thoughtworks.DDF.Option

trait None extends OptionType {
  def none[A <: Type: Kind]: Option[A]
}
