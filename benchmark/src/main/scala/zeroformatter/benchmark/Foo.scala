package zeroformatter.benchmark

import cats.kernel.Eq
import zeroformatter._

case class Foo(
  @Index(0) s: String,
  @Index(1) d: Double,
  @Index(2) i: Int,
  @Index(3) l: Long,
  @Index(4) bs: Vector[Boolean]
)

object Foo extends ZeroFormatterFooInstances {
  implicit val eqFoo: Eq[Foo] = Eq.fromUniversalEquals[Foo]
}
