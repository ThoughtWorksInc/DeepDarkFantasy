package com.thoughtworks.DDF

trait BCKWLang[Info[_], Repr[_]] extends
  BLang[Info, Repr] with
  CLang[Info, Repr] with
  KLang[Info, Repr] with
  WLang[Info, Repr]
