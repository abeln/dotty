package dotty.tools.backend.sjs

import dotty.tools.dotc.core._
import Contexts._
import dotty.tools.dotc.util.Spans
import Spans.Span

import org.scalajs.ir

import scala.ExplicitNulls._

/** Conversion utilities from dotty Positions to IR Positions. */
class JSPositions()(implicit ctx: Context) {

  /** Implicit conversion from dotty Position to ir.Position. */
  implicit def pos2irPos(span: Span): ir.Position = {
    if (!span.exists) ir.Position.NoPosition
    else {
      val source = pos2irPosCache.toIRSource(ctx.compilationUnit.source)
      val sourcePos = ctx.compilationUnit.source.atSpan(span)
      // dotty positions are 1-based but IR positions are 0-based
      ir.Position(source, sourcePos.line-1, sourcePos.column-1)
    }
  }

  /** Implicitly materializes an ir.Position from an implicit dotty Position. */
  implicit def implicitPos2irPos(
      implicit span: Span): ir.Position = {
    pos2irPos(span)
  }

  private[this] object pos2irPosCache { // scalastyle:ignore
    import dotty.tools.dotc.util._

    private[this] var lastDotcSource: Nullable[SourceFile] = null
    private[this] var lastIRSource: Nullable[ir.Position.SourceFile] = null

    def toIRSource(dotcSource: SourceFile): ir.Position.SourceFile = {
      if (dotcSource != lastDotcSource) {
        lastIRSource = convert(dotcSource)
        lastDotcSource = dotcSource
      }
      lastIRSource.nn
    }

    private[this] def convert(dotcSource: SourceFile): ir.Position.SourceFile = {
      dotcSource.file.file match {
        case null =>
          new java.net.URI(
              "virtualfile",        // Pseudo-Scheme
              dotcSource.file.path, // Scheme specific part
              null                  // Fragment
          )
        case file =>
          val srcURI = file.nn.toURI
          def matches(pat: java.net.URI) = pat.relativize(srcURI) != srcURI

          // TODO
          /*scalaJSOpts.sourceURIMaps.collectFirst {
            case ScalaJSOptions.URIMap(from, to) if matches(from) =>
              val relURI = from.relativize(srcURI)
              to.fold(relURI)(_.resolve(relURI))
          } getOrElse*/ srcURI
      }
    }
  }
}
