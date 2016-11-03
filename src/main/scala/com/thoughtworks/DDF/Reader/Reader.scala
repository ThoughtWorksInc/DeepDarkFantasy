package com.thoughtworks.DDF.Reader

trait Reader[Info[_], Repr[_]] extends ReaderRet[Info, Repr] with ReaderBind[Info, Repr]