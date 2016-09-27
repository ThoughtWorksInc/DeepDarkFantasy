package com.thoughtworks.DDF.Combinators

trait Comb[Info[_], Repr[_]] extends SKILang[Info, Repr] with BCKWLang[Info, Repr] with YLang[Info, Repr]
