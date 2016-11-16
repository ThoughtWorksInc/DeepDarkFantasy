package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bool.Bool
import com.thoughtworks.DDF.Bot.BotMin
import com.thoughtworks.DDF.Combinators.{SKI, Y}
import com.thoughtworks.DDF.Double.DoubleMin
import com.thoughtworks.DDF.List.ListMin
import com.thoughtworks.DDF.Option.Option
import com.thoughtworks.DDF.Product.ProdMin
import com.thoughtworks.DDF.Sum.Sum
import com.thoughtworks.DDF.Top.Top
import com.thoughtworks.DDF.Stream.Stream

trait MinLang[Info[_], Repr[_]] extends
  ProdMin[Info, Repr] with
  LangInfo[Info, Repr] with
  ListMin[Info, Repr] with
  DoubleMin[Info, Repr] with
  Option[Info, Repr] with
  Top[Info, Repr] with
  Bool[Info, Repr] with
  Sum[Info, Repr] with
  BotMin[Info, Repr] with
  SKI[Info, Repr] with
  Y[Info, Repr] with
  Stream[Info, Repr]
