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

trait Lang[Info[_], Repr[_]] extends
  LangInfo[Info, Repr] with
  InterLang[Info, Repr] with
  Prod[Info, Repr] with
  Double[Info, Repr] with
  Option[Info, Repr] with
  Except[Info, Repr] with
  Reader[Info, Repr] with
  State[Info, Repr] with
  List[Info, Repr] with
  Bool[Info, Repr] with
  Comb[Info, Repr] with
  Cont[Info, Repr] with
  Sum[Info, Repr] with
  Top[Info, Repr] with
  Bot[Info, Repr] with
  IO[Info, Repr] with
  Stream[Info, Repr]
