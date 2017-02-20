package com.thoughtworks.DDF.String

import com.thoughtworks.DDF.Arrow.Arr

trait String extends StringType with Arr {
  def litString: scala.Predef.String => String

  def stringApp: String ~>: String ~>: String

  def stringApp_ : String => String ~>: String = app(stringApp)

  def stringApp__ : String => String => String = l => app(stringApp_(l))
}
