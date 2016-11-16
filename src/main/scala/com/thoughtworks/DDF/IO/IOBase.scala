package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.Double.DoubleMin
import com.thoughtworks.DDF.Top.Top

trait IOBase[Info[_], Repr[_]] extends
  IOInfo[Info, Repr] with
  DoubleMin[Info, Repr] with
  Top[Info, Repr]
