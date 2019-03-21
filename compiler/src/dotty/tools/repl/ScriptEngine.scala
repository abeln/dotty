package dotty.tools
package repl

import java.io.Reader
import javax.script.{AbstractScriptEngine, Bindings, ScriptContext, ScriptEngine => JScriptEngine, ScriptEngineFactory, ScriptException, SimpleBindings}
import dotc.core.StdNames.str
import scala.ExplicitNulls._

/** A JSR 223 (Scripting API) compatible wrapper around the REPL for improved
 *  interoperability with software that supports it.
 *
 *  It works by instantiating a new script engine through the script engine manager.
 *  The script engine provides a eval method to evaluate scripts in string form.
 *  Example use:
 *
 *  val m = new javax.script.ScriptEngineManager()
 *  val e = m.getEngineByName("scala")
 *  println(e.eval("42"))
 */
class ScriptEngine extends AbstractScriptEngine {
  private[this] val driver = new ReplDriver(Array("-usejavacp", "-color:never"), Console.out, None)
  private[this] val rendering = new Rendering
  private[this] var state: State = driver.initialState

  def getFactory: ScriptEngineFactory = new ScriptEngine.Factory

  def createBindings: Bindings = new SimpleBindings

  /* Evaluate with the given context. */
  @throws[ScriptException]
  def eval(script: String, context: ScriptContext): Object = {
    val vid = state.valIndex
    state = driver.run(script)(state)
    val oid = state.objectIndex
    val res: Nullable[Object] =
      Class.forName(s"${str.REPL_SESSION_LINE}$oid", true, rendering.classLoader()(state.context))
        .getDeclaredMethods.find(_.nn.getName == s"${str.REPL_RES_PREFIX}$vid")
        .map(_.nn.invoke(null))
        .getOrElse(null)
    // TODO(abeln): hack because we can't make the method return `Nullable[Object]` (because of our limitations with Nullable and overriding).
    res.asInstanceOf[Object]
  }

  @throws[ScriptException]
  def eval(reader: Reader, context: ScriptContext): Object = throw new UnsupportedOperationException
}

object ScriptEngine {
  import java.util.Arrays
  import scala.util.Properties

  class Factory extends ScriptEngineFactory {
    def getEngineName = "Scala REPL"
    def getEngineVersion = "3.0"
    def getExtensions = Arrays.asList("scala")
    def getLanguageName = "Scala"
    def getLanguageVersion = Properties.versionString
    def getMimeTypes = Arrays.asList("application/x-scala")
    def getNames = Arrays.asList("scala")

    def getMethodCallSyntax(obj: String, m: String, args: String*) = s"$obj.$m(${args.mkString(", ")})"

    def getOutputStatement(toDisplay: String) = s"""print("$toDisplay")"""

    def getParameter(key: String): Object = {
      val res: Nullable[Object] = key match {
        case JScriptEngine.ENGINE           => getEngineName
        case JScriptEngine.ENGINE_VERSION   => getEngineVersion
        case JScriptEngine.LANGUAGE         => getLanguageName
        case JScriptEngine.LANGUAGE_VERSION => getLanguageVersion
        case JScriptEngine.NAME             => getNames.get(0)
        case _ => null
      }
      // TODO(abeln): hack
      res.asInstanceOf[Object]
    }

    def getProgram(statements: String*) = statements.mkString("; ")

    def getScriptEngine: JScriptEngine = new ScriptEngine
  }
}
