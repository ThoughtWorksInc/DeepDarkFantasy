package com.thoughtworks.DDF.IO

trait GetDouble extends IOBase {
  def getDouble: IO[Double]
}
