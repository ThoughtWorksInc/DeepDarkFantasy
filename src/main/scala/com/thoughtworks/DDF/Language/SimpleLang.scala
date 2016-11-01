package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Arrow.SimpleArr
import com.thoughtworks.DDF.Bool.SimpleBool
import com.thoughtworks.DDF.Bot.SimpleBot
import com.thoughtworks.DDF.Double.SimpleDouble
import com.thoughtworks.DDF.List.SimpleList
import com.thoughtworks.DDF.NoInfo
import com.thoughtworks.DDF.Option.SimpleOption
import com.thoughtworks.DDF.Sum.SimpleSum
import com.thoughtworks.DDF.Top.SimpleTop

trait SimpleLang[Repr[_]] extends
  LangInfo[NoInfo, Repr] with
  SimpleArr[Repr] with
  SimpleOption[Repr] with
  SimpleDouble[Repr] with
  SimpleList[Repr] with
  SimpleSum[Repr] with
  SimpleBool[Repr] with
  SimpleTop[Repr] with
  SimpleBot[Repr]

object SimpleLang {
  implicit def apply[Repr[_]] = new SimpleLang[Repr] {}
}