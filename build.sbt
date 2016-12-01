lazy val root = (project in file(".")).
  settings(
    name := "LastFMBot",
    scalaVersion := "2.11.7",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
  )

