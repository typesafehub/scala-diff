package com.typesafe.diff

import java.io.{FileFilter, File}

object Main extends App {
  //val source = "../scala/build/pack"
  //val target = "../scala/build-sbt/pack"

  //val source = "../scala/dists/maven/2.11.8-20151113-143225-041cdfa86d/scala-actors/scala-actors.jar"
  //val target = "../scala/build-sbt/pack/lib/scala-actors.jar"

  val source = "../scala/dists/maven"
  val target = "../scala/dists/maven-sbt"

  //val source = "../scala/dists/maven/2.11.8-20151113-143225-041cdfa86d/scala-actors/scala-actors.jar"
  //val target = "../scala/target/scala-dist/scala-dist-2.11.8-SNAPSHOT.jar"

  val sourceF = new File(source)
  val targetF = new File(target)
  println(s"Comparing ${sourceF.getAbsolutePath}")
  println(s"       -> ${targetF.getAbsolutePath}")
  val dt = new DiffTool(sourceF, targetF)
  dt.diff
}
