package com.thoughtworks.DDF.List

trait List[Info[_], Repr[_]] extends
  ListMap[Info, Repr] with
  Reverse[Info, Repr] with
  FoldLeft[Info, Repr] with
  FoldRight[Info, Repr] with
  ListZip[Info, Repr] with
  ScanLeft[Info, Repr] with
  ScanRight[Info, Repr]