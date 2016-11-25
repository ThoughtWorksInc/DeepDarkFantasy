package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bool.ILIGBoolInfo
import com.thoughtworks.DDF.Bot.ILIGBotInfo
import com.thoughtworks.DDF.Double.ILIGDoubleInfo
import com.thoughtworks.DDF.IO.ILIGIOInfo
import com.thoughtworks.DDF.List.ILIGListInfo
import com.thoughtworks.DDF.Option.ILIGOptionInfo
import com.thoughtworks.DDF.Product.ILIGProdInfo
import com.thoughtworks.DDF.Stream.ILIGStreamInfo
import com.thoughtworks.DDF.Sum.ILIGSumInfo
import com.thoughtworks.DDF.Top.ILIGTopInfo

import scalaz.NaturalTransformation

trait ILIGLangInfo[R[_]] extends
  LangInfo[InterLangInfoG, R] with
  ILIGProdInfo[R] with
  ILIGDoubleInfo[R] with
  ILIGBoolInfo[R] with
  ILIGListInfo[R] with
  ILIGSumInfo[R] with
  ILIGTopInfo[R] with
  ILIGBotInfo[R] with
  ILIGOptionInfo[R] with
  ILIGIOInfo[R] with
  ILIGStreamInfo[R]

object ILIGLangInfo {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGLangInfo[Repr] =
    nt => new ILIGLangInfo[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}