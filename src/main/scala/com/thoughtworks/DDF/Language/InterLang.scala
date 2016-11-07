package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Arrow.Arr
import com.thoughtworks.DDF.Bool.Bool
import com.thoughtworks.DDF.Bot.BotMin
import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Double.Double
import com.thoughtworks.DDF.IO.IO
import com.thoughtworks.DDF.List.List
import com.thoughtworks.DDF.Option.Option
import com.thoughtworks.DDF.Product.Prod
import com.thoughtworks.DDF.Sum.Sum
import com.thoughtworks.DDF.Top.Top
import com.thoughtworks.DDF.Stream.Stream

trait InterLang[Info[_], Repr[_]] extends
  LangInfo[Info, Repr] with
  Prod[Info, Repr] with
  Double[Info, Repr] with
  Option[Info, Repr] with
  Arr[Info, Repr] with
  Top[Info, Repr] with
  List[Info, Repr] with
  Bool[Info, Repr] with
  Sum[Info, Repr] with
  Comb[Info, Repr] with
  BotMin[Info, Repr] with
  IO[Info, Repr] with
  Stream[Info, Repr]
