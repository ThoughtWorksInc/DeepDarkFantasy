package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bool.ILIGBoolInfo
import com.thoughtworks.DDF.Bot.ILIGBotInfo
import com.thoughtworks.DDF.Double.ILIGDoubleInfo
import com.thoughtworks.DDF.IO.ILIGIOInfo
import com.thoughtworks.DDF.List.ILIGListInfo
import com.thoughtworks.DDF.Option.ILIGOptionInfo
import com.thoughtworks.DDF.Product.ILIGProdType
import com.thoughtworks.DDF.Stream.ILIGStreamType
import com.thoughtworks.DDF.Sum.ILIGSumInfo
import com.thoughtworks.DDF.Top.ILIGTopType

import scalaz.NaturalTransformation

trait ILIGLangType[R[_]] extends
  LangType[InterLangInfoG, R] with
  ILIGProdType[R] with
  ILIGDoubleInfo[R] with
  ILIGBoolInfo[R] with
  ILIGListInfo[R] with
  ILIGSumInfo[R] with
  ILIGTopType[R] with
  ILIGBotInfo[R] with
  ILIGOptionInfo[R] with
  ILIGIOInfo[R] with
  ILIGStreamType[R]

object ILIGLangType {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGLangType[Repr] =
    nt => new ILIGLangType[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}