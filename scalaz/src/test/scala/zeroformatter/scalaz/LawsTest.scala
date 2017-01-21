package zeroformatter
package scalaz

import _root_.scalaz.std.anyVal._
import scalaprops._
import dog.props._

object LawsTest extends Base {

  val `Formatter laws` = Properties.list(
    scalazlaws.invariantFunctor.all[Formatter]
  ).lift()
}
