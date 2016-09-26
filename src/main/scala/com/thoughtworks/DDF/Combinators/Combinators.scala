package com.thoughtworks.DDF.Combinators

trait Combinators[Info[_], Repr[_]] extends SKILang[Info, Repr] with BCKWLang[Info, Repr]
