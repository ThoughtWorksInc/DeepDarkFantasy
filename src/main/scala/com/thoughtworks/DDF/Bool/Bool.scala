package com.thoughtworks.DDF.Bool

trait Bool[Info[_], Repr[_]] extends LitB[Info, Repr] with ITE[Info, Repr]