package com.thoughtworks.DDF

object Util {
  def combine[A]: Stream[A] => Stream[A] => Stream[A] = {
    case scala.Stream.Empty => r => r
    case lh #:: lt => {
      case scala.Stream.Empty => lh #:: lt
      case rh #:: rt => lh #:: rh #:: combine(lt)(rt)
    }
  }
}
