package dotty.tools.dotc.classpath

import dotty.tools.io.ClassRepresentation
import dotty.tools.io.{AbstractFile, VirtualDirectory}
import FileUtils._
import java.net.URL

import dotty.tools.io.ClassPath

import scala.ExplicitNulls._

case class VirtualDirectoryClassPath(dir: VirtualDirectory) extends ClassPath with DirectoryLookup[ClassFileEntryImpl] with NoSourcePaths {
  type F = AbstractFile

  // From AbstractFileClassLoader
  private final def lookupPath(base: AbstractFile)(pathParts: Seq[String], directory: Boolean): Nullable[AbstractFile] = {
    var file: Nullable[AbstractFile] = base
    for (dirPart <- pathParts.init) {
      file = file.nn.lookupName(dirPart, directory = true)
      if (file == null)
        return null
    }

    file.nn.lookupName(pathParts.last, directory = directory)
  }

  protected def emptyFiles: Array[AbstractFile] = Array.empty
  protected def getSubDir(packageDirName: String): Option[AbstractFile] =
    Option(lookupPath(dir)(packageDirName.split(java.io.File.separator), directory = true).asInstanceOf[AbstractFile])
  protected def listChildren(dir: AbstractFile, filter: Option[AbstractFile => Boolean] = None): Array[F] = filter match {
    case Some(f) => dir.iterator.filter(f).toArray
    case _ => dir.toArray
  }
  def getName(f: AbstractFile): String = f.name
  def toAbstractFile(f: AbstractFile): AbstractFile = f
  def isPackage(f: AbstractFile): Boolean = f.isPackage

  // mimic the behavior of the old nsc.util.DirectoryClassPath
  def asURLs: Seq[URL] = Seq(new URL(dir.name))
  def asClassPathStrings: Seq[String] = Seq(dir.path)

  override def findClass(className: String): Option[ClassRepresentation] = findClassFile(className) map ClassFileEntryImpl

  def findClassFile(className: String): Option[AbstractFile] = {
    val relativePath = FileUtils.dirPath(className) + ".class"
    Option(lookupPath(dir)(relativePath.split(java.io.File.separator), directory = false).asInstanceOf[AbstractFile])
  }

  private[dotty] def classes(inPackage: String): Seq[ClassFileEntry] = files(inPackage)

  protected def createFileEntry(file: AbstractFile): ClassFileEntryImpl = ClassFileEntryImpl(file)
  protected def isMatchingFile(f: AbstractFile): Boolean = f.isClass
}
