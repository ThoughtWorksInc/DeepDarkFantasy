package com.thoughtworks.DDF.State

trait State[Info[_], Repr[_]] extends StateRet[Info, Repr] with StateBind[Info, Repr]