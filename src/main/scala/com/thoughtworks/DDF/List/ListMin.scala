package com.thoughtworks.DDF.List

trait ListMin[Info[_], Repr[_]] extends Nil[Info, Repr] with Cons[Info, Repr] with ListMatch[Info, Repr]