package com.thoughtworks.DDF.IO

trait IO[Info[_], Repr[_]] extends
  IORet[Info, Repr] with
  IOBind[Info, Repr] with
  PutDouble[Info, Repr] with
  GetDouble[Info, Repr]