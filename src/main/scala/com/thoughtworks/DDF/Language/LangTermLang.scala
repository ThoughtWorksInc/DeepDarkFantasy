package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bot.LangTermBot
import com.thoughtworks.DDF.Combinators.LangTermComb
import com.thoughtworks.DDF.Cont.LangTermCont
import com.thoughtworks.DDF.Double.LangTermDouble
import com.thoughtworks.DDF.Except.LangTermExcept
import com.thoughtworks.DDF.IO.LangTermIO
import com.thoughtworks.DDF.List.LangTermList
import com.thoughtworks.DDF.Option.LangTermOption
import com.thoughtworks.DDF.Reader.LangTermReader
import com.thoughtworks.DDF.State.LangTermState
import com.thoughtworks.DDF.Stream.LangTermStream
import com.thoughtworks.DDF.String.LangTermString

trait LangTermLang extends
  Lang[LangInfoG, LangTerm] with
  LangTermBot with
  LangTermComb with
  LangTermDouble with
  LangTermOption with
  LangTermList with
  LangTermExcept with
  LangTermCont with
  LangTermIO with
  LangTermString with
  LangTermState with
  LangTermStream with
  LangTermReader

object LangTermLang extends LangTermLang