package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bool.BEvalBool
import com.thoughtworks.DDF.Combinators.BEvalComb
import com.thoughtworks.DDF.Double.BEvalDouble
import com.thoughtworks.DDF.List.BEvalList
import com.thoughtworks.DDF.Option.BEvalOption
import com.thoughtworks.DDF.Product.BEvalProduct
import com.thoughtworks.DDF.Sum.BEvalSum
import com.thoughtworks.DDF.Unit.BEvalUnit
import com.thoughtworks.DDF.{BEval, Loss}

trait BEvalLang extends
  Lang[Loss, BEval] with
  BEvalProduct with
  BEvalComb with
  BEvalDouble with
  BEvalSum with
  BEvalList with
  BEvalUnit with
  BEvalBool with
  BEvalOption

object BEvalLang {
  implicit def apply = new BEvalLang {}
}