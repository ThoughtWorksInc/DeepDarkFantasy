package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.Double.Double

trait IOBase[Info[_], Repr[_]] extends Double[Info, Repr] with IOInfo[Info, Repr]
