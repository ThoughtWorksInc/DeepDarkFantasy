package com.thoughtworks.DDF.Cont

trait Cont[Info[_], Repr[_]] extends ContRet[Info, Repr] with ContBind[Info, Repr]