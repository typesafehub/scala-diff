package com.typesafe.diff

import java.io.File

import sbt.FileFilter

object Filters {
  def regex(s: String): FileFilter = new FileFilter {
    val pat = s.r.pattern
    def accept(f: File) = pat.matcher(f.getAbsolutePath.replace('\\', '/')).matches()
  }

  def apply(pred: File => Boolean): FileFilter = new FileFilter {
    def accept(f: File) = pred(f)
  }
}
