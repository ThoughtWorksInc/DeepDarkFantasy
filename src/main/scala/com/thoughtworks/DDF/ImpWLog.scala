package com.thoughtworks.DDF

trait ImpWLog[X] {
  val exp: ImpW[X]

  trait Forward {
    val res: BEval[X]

    val log: Seq[String]

    def update(rate: Double)(tloss: Loss[X]): ImpWLog[X]
  }

  def forward: Forward
}
