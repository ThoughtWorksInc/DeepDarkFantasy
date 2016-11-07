package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Arrow.ArrInfo
import com.thoughtworks.DDF.Bool.BoolInfo
import com.thoughtworks.DDF.Bot.BotInfo
import com.thoughtworks.DDF.Double.DoubleInfo
import com.thoughtworks.DDF.IO.IOInfo
import com.thoughtworks.DDF.List.ListInfo
import com.thoughtworks.DDF.Option.OptionInfo
import com.thoughtworks.DDF.Product.ProdInfo
import com.thoughtworks.DDF.Stream.StreamInfo
import com.thoughtworks.DDF.Sum.SumInfo
import com.thoughtworks.DDF.Top.TopInfo

trait LangInfo[Info[_], Repr[_]] extends
  ProdInfo[Info, Repr] with
  DoubleInfo[Info, Repr] with
  OptionInfo[Info, Repr] with
  ArrInfo[Info, Repr] with
  TopInfo[Info, Repr] with
  ListInfo[Info, Repr] with
  BoolInfo[Info, Repr] with
  SumInfo[Info, Repr] with
  IOInfo[Info, Repr] with
  StreamInfo[Info, Repr] with
  BotInfo[Info, Repr]
