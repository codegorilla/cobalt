val scala3Version = "3.7.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "cobalt",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    // libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test,
      "org.freemarker" % "freemarker" % "2.3.34"
    )

  )
