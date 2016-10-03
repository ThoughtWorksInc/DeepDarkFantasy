package com.thoughtworks.DDF.Lang

import com.thoughtworks.DDF.Bool.EvalBool
import com.thoughtworks.DDF.Combinators.EvalComb
import com.thoughtworks.DDF.Double.EvalDouble
import com.thoughtworks.DDF.List.EvalList
import com.thoughtworks.DDF.Product.EvalProd
import com.thoughtworks.DDF.Sum.EvalSum
import com.thoughtworks.DDF.{Eval, Loss}

trait EvalLang extends
  Lang[Loss, Eval] with
  EvalProd with
  EvalComb with
  EvalDouble with
  EvalSum with
  EvalList with
  EvalBool

object EvalLang {
  implicit def apply = new EvalLang {}
}