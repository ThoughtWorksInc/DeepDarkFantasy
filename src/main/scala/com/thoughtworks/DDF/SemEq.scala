package com.thoughtworks.DDF

import com.thoughtworks.DDF.Language.LangTerm

trait SemEq[A] {
  def semEq: LangTerm[A] => LangTerm[A] => Prop = _ => _ => new Prop { }
}
