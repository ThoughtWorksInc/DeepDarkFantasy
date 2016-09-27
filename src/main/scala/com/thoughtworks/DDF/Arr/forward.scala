package com.thoughtworks.DDF.Arr

import com.thoughtworks.DDF.Eval.{Eval, Loss}

trait forward[A, B] {

  trait backward[AL, BL] {
    val eb: Eval[B]

    def backward: BL => AL
  }

  def forward(ea: Eval[A])(implicit al: Loss[A], bl: Loss[B]): backward[al.loss, bl.loss]
}
