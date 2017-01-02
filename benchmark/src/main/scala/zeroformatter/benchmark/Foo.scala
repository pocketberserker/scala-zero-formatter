package zeroformatter.benchmark

import cats.kernel.Eq

case class Foo(s: String, d: Double, i: Int, l: Long, bs: List[Boolean])

object Foo extends ZeroFormatterFooInstances {
  implicit val eqFoo: Eq[Foo] = Eq.fromUniversalEquals[Foo]
}
