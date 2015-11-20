name := "scala-diff"

scalaVersion := "2.11.7"

libraryDependencies += "com.googlecode.java-diff-utils" % "diffutils" % "1.2.1"

resolvers += Resolver.url("typesafe-ivy-repo", url("http://typesafe.artifactoryonline.com/typesafe/releases"))(Resolver.ivyStylePatterns)

libraryDependencies += "org.scala-sbt" %% "io" % "0.13.9"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5"
