package zeroformatter
package scalaz

import _root_.scalaz._
import _root_.scalaz.std.anyVal._
import _root_.scalaz.IList._
import dog.TestCase

object FormatterTest extends Base {

  val `serialize and deserialize IList[Int]` = TestCase {
    for {
      values <- SequenceFormatterTest.`serialize Array[Int]`
      value = IList.fromList(values._1.toList)
      bytes = values._2
      actualBytes = zeroformatter.ZeroFormatter.serialize(value)
      _ <- assert.eq(bytes, actualBytes).lift
      _ <- assert.eq(value, zeroformatter.ZeroFormatter.deserialize[IList[Int]](actualBytes)).lift
    } yield ()
  }
}
