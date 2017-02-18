package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Arrow.ArrType
import com.thoughtworks.DDF.Bool.BoolType
import com.thoughtworks.DDF.Bot.BotType
import com.thoughtworks.DDF.Double.DoubleType
import com.thoughtworks.DDF.IO.IOType
import com.thoughtworks.DDF.List.ListType
import com.thoughtworks.DDF.Option.OptionType
import com.thoughtworks.DDF.Product.ProdType
import com.thoughtworks.DDF.Stream.StreamType
import com.thoughtworks.DDF.Sum.SumType
import com.thoughtworks.DDF.Top.TopType

trait LangType extends
  ProdType with
  DoubleType with
  OptionType with
  ArrType with
  TopType with
  ListType with
  BoolType with
  SumType with
  IOType with
  StreamType with
  BotType
