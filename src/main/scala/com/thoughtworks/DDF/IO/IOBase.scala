package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.Arrow.ArrMin

trait IOBase[Info[_], Repr[_]] extends ArrMin[Info, Repr] with IOInfo[Info, Repr]
