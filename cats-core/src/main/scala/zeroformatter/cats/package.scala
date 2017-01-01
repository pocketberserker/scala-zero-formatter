package zeroformatter

import _root_.cats.functor._

package object cats {

  implicit val formatterInvariantInstance: Invariant[Formatter] = new Invariant[Formatter] {
    def imap[A, B](fa: Formatter[A])(f: A => B)(g: B => A): Formatter[B] = fa.xmap(f, g)
  }
}
