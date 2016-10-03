package com.thoughtworks.DDF.Lang

import com.thoughtworks.DDF.Bool.EvalBool
import com.thoughtworks.DDF.Combinators.EvalComb
import com.thoughtworks.DDF.Double.EvalDouble
import com.thoughtworks.DDF.List.EvalList
import com.thoughtworks.DDF.Product.EvalProduct
import com.thoughtworks.DDF.Sum.EvalSum
import com.thoughtworks.DDF.Unit.EvalUnit
import com.thoughtworks.DDF.{Eval, Loss}

trait EvalLang extends
  Lang[Loss, Eval] with
  EvalProduct with
  EvalComb with
  EvalDouble with
  EvalSum with
  EvalList with
  EvalUnit with
  EvalBool

object EvalLang {
  implicit def apply = new EvalLang {}
}