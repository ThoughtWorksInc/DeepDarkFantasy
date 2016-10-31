package com.thoughtworks.DDF.Sum

trait SumMin[Info[_], Repr[_]] extends Left[Info, Repr] with Right[Info, Repr] with SumMatch[Info, Repr]