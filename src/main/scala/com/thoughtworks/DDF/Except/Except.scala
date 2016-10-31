package com.thoughtworks.DDF.Except

trait Except[Info[_], Repr[_]] extends ExceptRet[Info, Repr] with ExceptBind[Info, Repr]