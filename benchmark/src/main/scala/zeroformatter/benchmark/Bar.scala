package zeroformatter.benchmark

import cats.Eval
import zeroformatter._

case class Bar(
  @Index(0) s: Eval[String],
  @Index(1) d: Eval[Double],
  @Index(2) i: Eval[Int],
  @Index(3) l: Eval[Long],
  @Index(4) bs: Eval[List[Boolean]]
)

object Bar extends ZeroFormatterBarInstances {
}