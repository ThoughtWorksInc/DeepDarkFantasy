package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bot.IsoBot
import com.thoughtworks.DDF.Combinators.IsoComb
import com.thoughtworks.DDF.Cont.IsoCont
import com.thoughtworks.DDF.Except.IsoExcept
import com.thoughtworks.DDF.IO.IsoIO
import com.thoughtworks.DDF.List.IsoList
import com.thoughtworks.DDF.Option.IsoOption
import com.thoughtworks.DDF.Reader.IsoReader
import com.thoughtworks.DDF.State.IsoState
import com.thoughtworks.DDF.Stream.IsoStream
import com.thoughtworks.DDF.String.IsoString
import scalaz.Isomorphism._

trait IsoLang[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  Lang[NInfo, NRepr] with
  IsoList[OInfo, NInfo, ORepr, NRepr] with
  IsoComb[OInfo, NInfo, ORepr, NRepr] with
  IsoCont[OInfo, NInfo, ORepr, NRepr] with
  IsoOption[OInfo, NInfo, ORepr, NRepr] with
  IsoBot[OInfo, NInfo, ORepr, NRepr] with
  IsoExcept[OInfo, NInfo, ORepr, NRepr] with
  IsoReader[OInfo, NInfo, ORepr, NRepr] with
  IsoState[OInfo, NInfo, ORepr, NRepr] with
  IsoIO[OInfo, NInfo, ORepr, NRepr] with
  IsoStream[OInfo, NInfo, ORepr, NRepr] with
  IsoString[OInfo, NInfo, ORepr, NRepr]

object IsoLang {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoLang[OInfo, NInfo, ORepr, NRepr] =
    new IsoLang[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}
