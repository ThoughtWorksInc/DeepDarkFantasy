package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Arrow.Arr
import com.thoughtworks.DDF.Bool.Bool
import com.thoughtworks.DDF.Bot.BotMin
import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Double.DoubleMin
import com.thoughtworks.DDF.IO.IO
import com.thoughtworks.DDF.List.List
import com.thoughtworks.DDF.Option.Option
import com.thoughtworks.DDF.Product.ProdMin
import com.thoughtworks.DDF.Sum.Sum
import com.thoughtworks.DDF.Top.Top
import com.thoughtworks.DDF.Stream.Stream
import com.thoughtworks.DDF.String.String

trait InterLang extends
  LangType with
  ProdMin with
  DoubleMin with
  Option with
  Arr with
  Top with
  List with
  Bool with
  Sum with
  Comb with
  BotMin with
  IO with
  Stream with
  String
