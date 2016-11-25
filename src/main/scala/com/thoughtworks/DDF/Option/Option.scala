package com.thoughtworks.DDF.Option

trait Option[Info[_], Repr[_]] extends None[Info, Repr] with Some[Info, Repr] with OptionMatch[Info, Repr]