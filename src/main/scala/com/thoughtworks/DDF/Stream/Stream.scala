package com.thoughtworks.DDF.Stream

trait Stream[Info[_], Repr[_]] extends StreamNil[Info, Repr] with StreamCons[Info, Repr] with StreamMatch[Info, Repr]