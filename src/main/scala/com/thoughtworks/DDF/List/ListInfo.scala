package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Product.ProductInfo

trait ListInfo[Info[_], Repr[_]] extends ProductInfo[Info, Repr] {
  implicit def listInfo[A](implicit ai: Info[A]): Info[List[A]]

  def listElmInfo[A](implicit lai: Info[List[A]]): Info[A]
}
