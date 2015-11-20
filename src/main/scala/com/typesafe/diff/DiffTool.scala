package com.typesafe.diff

import java.io._
import java.util.jar.Manifest
import java.util.{Properties, Arrays}
import java.util.zip.{ZipEntry, ZipFile}

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml._

import sbt.FileFilter._
import sbt.{NameFilter, Using, IO}

import difflib.DiffUtils

class DiffTool(startFrom: File, startTo: File) {
  val compareContents: Boolean = true
  val ignoredFiles: FileFilter = (
    ".DS_Store" || "*.complete" ||
    // The ant build produces a file `package-info.class` in the forkjoin project. Since the
    // corresponding `package-info.java` does not contain any annotation, this is probably a bug.
    "package-info.class" ||
    Filters(f => f.isDirectory && f.list().isEmpty)
  )
  val ignoredContents: NameFilter = "*.class" // | "*.html" | "*.js"
  val ignoredManifestAttrs: NameFilter = "manifest-version" | "ant-version" | "created-by" | "bnd-lastmodified" | "tool"
  val attributedListManifestAttrs: NameFilter = "export-package" | "import-package"
  val listManifestAttrs: NameFilter = "bundle-requiredexecutionenvironment"
  val pomFiles: NameFilter = "*-pom.xml" | "*.pom"
  val xmlFiles: NameFilter = "*.xml"
  val textFiles: NameFilter = "*.MF" | "*.1" | "*.scala" | "*.java" | "*.html" | "*.js"
  val showUnifiedDiff: Boolean = true
  val pomReplace: Map[String, String] = Map(
    "@VERSION@" -> "2.11.8-SNAPSHOT",
    "@SCALA_BINARY_VERSION@" -> "2.11",
    "@SCALA_FULL_VERSION@" -> "2.11.7",
    "@CONTINUATIONS_PLUGIN_VERSION@" -> "1.0.2",
    "@CONTINUATIONS_LIBRARY_VERSION@" -> "1.0.2",
    "@SCALA_SWING_VERSION@" -> "1.0.2",
    "@ACTORS_MIGRATION_VERSION@" -> "1.1.0",
    "@AKKA_ACTOR_VERSION@" -> "2.3.10",
    "@JLINE_VERSION@" -> "2.12.1",
    "@XML_VERSION@" -> "1.0.4",
    "@PARSER_COMBINATORS_VERSION@" -> "1.0.4"
  )
  val pomIgnoreNamespaces: Boolean = true

  val startFromAbs = startFrom.getAbsoluteFile
  val startToAbs = startTo.getAbsoluteFile

  val commonBase: Option[File] = {
    val p1 = Iterator.iterate(startFromAbs)(_.getParentFile).takeWhile(_ ne null).toList.reverse
    val p2 = Iterator.iterate(startToAbs)(_.getParentFile).takeWhile(_ ne null).toList.reverse
    (p1, p2).zipped.takeWhile(p => p._1 == p._2).lastOption.map(_._1)
  }

  def showPath(f: File): String =
    commonBase.flatMap(IO.relativize(_, f)).getOrElse(f.toString)

  def diff: Unit = diff(startFromAbs, startToAbs)

  def diff(from: File, to: File): Unit = {
    if(ignoredFiles.accept(from) || ignoredFiles.accept(to)) {
      //println(s"Ignoring $from -> $to")
    } else if(from.isDirectory && to.isDirectory) diffDir(from, to)
    else if(from.isDirectory || to.isDirectory)
      println(s"Cannot compare directory and file: $from -> $to")
    else {
      val n = from.getName.toLowerCase
      if(n.endsWith(".zip") || n.endsWith(".jar"))
        Using.zipFile(from)(f => Using.zipFile(to)(t => diffZip(from, f, to, t)))
      else diffLeaf(from.getName, from, to, None, None)
    }
  }

  def diffDir(from: File, to: File): Unit = {
    val fromCh = from.listFiles.map(f => f.getName -> f).toMap
    val toCh = to.listFiles.map(f => f.getName -> f).toMap
    val allNames = (fromCh.keySet ++ toCh.keySet)
    val onlyFromNames = (allNames -- toCh.keySet)
    val onlyToNames = (allNames -- fromCh.keySet)
    val pairedNames = allNames -- (onlyToNames ++ onlyFromNames)
    val onlyFromFiles = onlyFromNames.map(new File(from, _)).filterNot(ignoredFiles.accept _)
    val onlyToFiles = onlyToNames.map(new File(to, _)).filterNot(ignoredFiles.accept _)
    if(onlyFromFiles.nonEmpty)
      println(s"Files in ${showPath(from)} but not in ${showPath(to)}: " + onlyFromFiles.map(_.getName).mkString(", "))
    if(onlyToFiles.nonEmpty)
      println(s"Files not in ${showPath(from)} but in ${showPath(to)}: " + onlyToFiles.map(_.getName).mkString(", "))
    pairedNames.toSeq.sorted.foreach { n => diff(fromCh(n), toCh(n)) }
  }

  def diffZip(fromF: File, from: ZipFile, toF: File, to: ZipFile): Unit = {
    val fromCh = eliminateParentDirs(from.entries().map(e => e.getName -> e).toMap).filterKeys(n => !ignoredFiles.accept(new File(n)))
    val toCh = eliminateParentDirs(to.entries().map(e => e.getName -> e).toMap).filterKeys(n => !ignoredFiles.accept(new File(n)))
    val allNames = (fromCh.keySet ++ toCh.keySet)
    val onlyFromNames = (allNames -- toCh.keySet)
    val onlyToNames = (allNames -- fromCh.keySet)
    val pairedNames = allNames -- (onlyToNames ++ onlyFromNames)
    val onlyFromEntries = onlyFromNames.map(fromCh)
    val onlyToEntries = onlyToNames.map(toCh)
    if(onlyFromEntries.nonEmpty)
      println(s"Zip entries in ${showPath(fromF)} but not in ${showPath(toF)}: " + onlyFromEntries.map(_.getName).mkString(", "))
    if(onlyToEntries.nonEmpty)
      println(s"Zip entries not in ${showPath(fromF)} but in ${showPath(toF)}: " + onlyToEntries.map(_.getName).mkString(", "))
    pairedNames.toSeq.sorted.foreach { n =>
      diffLeaf(n, fromF, toF, Some((from, fromCh(n))), Some((to, toCh(n))))
    }
  }

  def diffLeaf(fileName: String, fromF: File, toF: File, fromZip: Option[(ZipFile, ZipEntry)], toZip: Option[(ZipFile, ZipEntry)]): Unit = {
    if(compareContents && !ignoredContents.accept(fileName)) {
      (fromZip, toZip) match {
        case (Some((fromZ, fromE)), Some((toZ, toE))) =>
          diffData(showPath(toF) + "!/" + toE.getName, IO.readBytes(fromZ.getInputStream(fromE)), IO.readBytes(toZ.getInputStream(toE)))
        case _ =>
          diffData(showPath(toF), IO.readBytes(fromF), IO.readBytes(toF))
      }
    }
  }

  def diffData(name: String, from: Array[Byte], to: Array[Byte]): Unit = {
    if(name.endsWith(".properties")) {
      val p1 = new Properties
      val p2 = new Properties
      p1.load(new ByteArrayInputStream(from))
      p2.load(new ByteArrayInputStream(to))
      if(!p1.equals(p2)) {
        println(s"Properties files do not match: $name")
        diffMap(p1.toMap, p2.toMap).dump
      }
    } else if(name.endsWith("MANIFEST.MF")) {
      def attrs(data: Array[Byte]): Map[String, String] =
        (new Manifest(new ByteArrayInputStream(data))).getMainAttributes.entrySet()
          .map(e => e.getKey.toString.toLowerCase -> e.getValue.toString).toMap
          .collect {
            case (k, v) if !ignoredManifestAttrs.accept(k) =>
              val v2 =
                if(attributedListManifestAttrs.accept(k))
                  splitListHeader(v, ',').map { s =>
                    val all = splitListHeader(s, ';')
                    (if(all.length > 1) all.head +: all.tail.sorted
                     else all).mkString(";")
                  }.sorted.mkString(", ")
                else if(listManifestAttrs.accept(k)) splitListHeader(v, ',').sorted.mkString(", ")
                else v
              (k, v2)
          }
      val map1 = attrs(from)
      val map2 = attrs(to)
      if(!map1.equals(map2)) {
        println(s"Manifest files do not match: $name")
        diffMap(map1, map2).dump
      }
    } else {
      if(!Arrays.equals(from, to)) {
        val isPom = pomFiles.accept(name)
        if(isPom || xmlFiles.accept(name)) {
          def normalize(data: Array[Byte]): Seq[String] = {
            val doc = XML.loadString(new String(data, "UTF-8"))
            val pp = new PrettyPrinter(Int.MaxValue, 2)
            val out = (if(isPom) {
              val doc2 = if(isPom) {
                def clearScope(x: Node):Node = x match {
                  case e:Elem => e.copy(scope=TopScope, child = e.child.map(clearScope))
                  case o => o
                }
                clearScope(doc.copy(attributes = doc.attributes.filter {
                  case a: Attribute if a.pre == "xsi" || a.pre == "xmlns" || a.key == "xmlns" => false
                  case a => true
                }))
              } else doc
              val doc3 = (new RuleTransformer(new RewriteRule {
                override def transform(node: Node) = node match {
                  case e: Elem if e.label == "project" =>
                    e.copy(child = e.child.sortBy(_.label))
                  case n => n
                }
              })).transform(doc2).head
              pomReplace.foldLeft(pp.format(doc3)) { case (s, (k, v)) => s.replace(k, v) }
            } else pp.format(doc))
            out.replace("\r", "").split("\n")
          }
          val d = diffLines(normalize(from), normalize(to))
          if(d.differs) {
            val tpe = if(isPom) "POM" else "XML"
            println(s"$tpe files do not match: $name")
            d.dump
          }
        } else if(textFiles.accept(name)) {
          println(s"Text files do not match: $name")
          if(!diffLines(
            IO.readLines(new BufferedReader(new InputStreamReader(new ByteArrayInputStream(from), "UTF-8"))),
            IO.readLines(new BufferedReader(new InputStreamReader(new ByteArrayInputStream(to), "UTF-8")))
          ).dump) {
            println("  No changes -- Performing EOL-aware diff")
            if(!diffLines(
              (new String(from, "UTF-8")).replace("\r", "<CR>").split('\n'),
              (new String(to, "UTF-8")).replace("\r", "<CR>").split('\n')
            ).dump) println("  No changes -- Assuming UTF encoding differences")
          }
        } else println(s"Binary files do not match: $name")
      }
    }
  }

  def diffMap(from: Map[String, String], to: Map[String, String]): Diff =
    diffLines(from.toSeq.sortBy(_._1).map { case (k, v) => s"$k: $v" },
      to.toSeq.sortBy(_._1).map { case (k, v) => s"$k: $v" }, 0)

  def diffLines(from: Seq[String], to: Seq[String], context: Int = 1): Diff = {
    val patch = DiffUtils.diff(from, to)
    Diff(if(patch.getDeltas.isEmpty) Nil else {
      if(showUnifiedDiff) DiffUtils.generateUnifiedDiff("from", "to", from, patch, context).drop(2).toSeq
      else patch.getDeltas.map(_.toString).toSeq
    })
  }

  /** Eliminate zip entries for non-empty parent dirs */
  def eliminateParentDirs(s: Map[String, ZipEntry]): Map[String, ZipEntry] = {
    val parents = s.keySet.map { n =>
      val n2 = if(n.endsWith("/")) n.substring(0, n.length-1) else n
      val pos = n2.lastIndexOf('/')
      if(pos == -1) n2 else n2.substring(0, pos+1)
    }
    s.filterNot { case (k, v) => v.isDirectory && parents.contains(k) }
  }

  def splitListHeader(s: String, sep: Char): Seq[String] = {
    val res = new ArrayBuffer[String]
    var current = ""
    val len = s.length
    var i = 0
    var inQuote = false
    while(i < len) {
      s(i) match {
        case c if c == sep && !inQuote =>
          if(!current.isEmpty) {
            res.append(current.trim)
            current = ""
          }
        case '"' =>
          inQuote = !inQuote
          current += '"'
        case c =>
          current += c
      }
      i += 1
    }
    if(!current.isEmpty) res.append(current.trim)
    res
  }
}

case class Diff(data: Seq[String]) {
  def differs = data.nonEmpty
  def dump: Boolean = {
    data.foreach(s => println("  " + s))
    differs
  }
}