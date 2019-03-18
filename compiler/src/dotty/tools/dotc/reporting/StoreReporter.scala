package dotty.tools
package dotc
package reporting

import core.Contexts.Context
import collection.mutable
import config.Printers.typr
import diagnostic.MessageContainer
import diagnostic.messages._

import scala.ExplicitNulls._

/** This class implements a Reporter that stores all messages
  *
  * Beware that this reporter can leak memory, and force messages in two
  * scenarios:
  *
  * - During debugging `config.Printers.typr` is set from `noPrinter` to `new
  *   Printer`, which forces the message
  * - The reporter is not flushed and the message containers capture a
  *   `Context` (about 4MB)
  */
class StoreReporter(outer: Nullable[Reporter]) extends Reporter {

  protected[this] var infos: Nullable[mutable.ListBuffer[MessageContainer]] = null

  def doReport(m: MessageContainer)(implicit ctx: Context): Unit = {
    typr.println(s">>>> StoredError: ${m.message}") // !!! DEBUG
    if (infos == null) infos = new mutable.ListBuffer
    infos.nn += m
  }

  override def hasUnreportedErrors: Boolean =
    outer != null && infos != null && infos.nn.exists(_.isInstanceOf[Error])

  override def removeBufferedMessages(implicit ctx: Context): List[MessageContainer] =
    if (infos != null) try infos.nn.toList finally infos = null
    else Nil

  override def errorsReported: Boolean = hasErrors || (outer != null && outer.errorsReported)
}
