package com.thoughtworks.DDF

trait ImpWLog[X] {
  val exp: ImpW[X]

  trait Forward {
    val res: BEval[X]

    val log: Seq[String]

    def update[XL](rate: Double, tloss: XL)(implicit xi: LossInfo.Aux[X, XL]): ImpWLog[X]
  }

  def forward: Forward
}
