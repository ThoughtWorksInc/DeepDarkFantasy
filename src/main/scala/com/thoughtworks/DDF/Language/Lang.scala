package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bool.Bool
import com.thoughtworks.DDF.Bot.Bot
import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Cont.Cont
import com.thoughtworks.DDF.Double.Double
import com.thoughtworks.DDF.Except.Except
import com.thoughtworks.DDF.IO.IO
import com.thoughtworks.DDF.List.List
import com.thoughtworks.DDF.Option.Option
import com.thoughtworks.DDF.Product.Prod
import com.thoughtworks.DDF.Reader.Reader
import com.thoughtworks.DDF.State.State
import com.thoughtworks.DDF.Sum.Sum
import com.thoughtworks.DDF.Top.Top
import com.thoughtworks.DDF.Stream.Stream
import com.thoughtworks.DDF.String.String

trait Lang extends
  LangType with
  Prod with
  Double with
  Option with
  Except with
  Reader with
  State with
  List with
  Bool with
  Comb with
  Cont with
  Sum with
  Top with
  Bot with
  IO with
  Stream with
  String
