package com.thoughtworks.DDF.Sum

trait Sum[Info[_], Repr[_]] extends SumComm[Info, Repr] with SumAssocLR[Info, Repr] with SumAssocRL[Info, Repr]