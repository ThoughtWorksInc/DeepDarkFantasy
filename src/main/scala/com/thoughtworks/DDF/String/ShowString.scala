package com.thoughtworks.DDF.String

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowString extends String[NoInfo, Lambda[X => Show]] with ShowArr with SimpleString[Lambda[X => Show]] {
  override def litString = str => Show("str: " + str)

  override def stringApp = Show("stringApp")
}

object ShowString extends ShowString