package com.thoughtworks.DDF

trait ImpWLog[X] {
  val exp: ImpW[X]
  def update[XL](rate: Double, xl: XL)(implicit ti: Loss.Aux[X, XL]): ImpWLog[X]
}
